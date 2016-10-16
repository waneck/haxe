(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Ast
open Globals
open Type
open Object
open IO
open ExtString
open ExtList

let rec normalize_type t =
	match t with
		| TMono tref -> (match !tref with
			| None -> t
			| Some t -> normalize_type t)
		| TEnum(e,params) ->
			TEnum(e, List.map normalize_type params)
		| TInst(c,params) ->
			TInst(c, List.map normalize_type params)
		| TType(t,params) ->
			TType(t, List.map normalize_type params)
		| TFun(args,ret) ->
			TFun(List.map (fun (n,o,t) -> (n,o,normalize_type t)) args, normalize_type ret)
		| TAnon(anon) ->
			TAnon(anon)
		| TDynamic(oft) ->
			if t == t_dynamic then
				t_dynamic
			else
				TDynamic(normalize_type oft)
		| TLazy(tlazy) ->
			normalize_type (!tlazy())
		| TAbstract(a,params) ->
			TAbstract(a, List.map normalize_type params)

module TypeHash =
struct
	type t = Type.t

	let rec equal i j = (match normalize_type i, normalize_type j with
		| TMono t1, TMono t2 ->
			t1 == t2
		| TEnum(e1, tl1), TEnum(e2, tl2) -> (try
				e1 == e2 && List.for_all2 equal tl1 tl2
			with | Invalid_argument("List.for_all2") -> false)
		| TInst(c1, tl1), TInst(c2, tl2) -> (try
				c1 == c2 && List.for_all2 equal tl1 tl2
			with | Invalid_argument("List.for_all2") -> false)
		| TType(t1, tl1), TType(t2, tl2) -> (try
				t1 == t2 && List.for_all2 equal tl1 tl2
			with | Invalid_argument("List.for_all2") -> false)
		| TFun(a1, r1), TFun(a2, r2) -> (try
				equal r1 r2 && List.for_all2 (fun (n1,o1,t1) (n2,o2,t2) ->
					n1 = n2 && o1 = o2 && equal t1 t2) a1 a2
			with | Invalid_argument("List.for_all2") -> false)
		| TAnon(a1), TAnon(a2) ->
			a1 == a2
		| TDynamic(t1), TDynamic(t2) ->
			t1 == t2 || (t1 != t_dynamic && t2 != t_dynamic && equal t1 t2)
		| TAbstract(a1, tl1), TAbstract(a2, tl2) -> (try
				a1 == a2 && List.for_all2 equal tl1 tl2
			with | Invalid_argument("List.for_all2") -> false)
		| _ -> false)

	let rec hash t =
		let fold_hash cur_hash t =
			31 * cur_hash + (hash t)
		in
		match normalize_type t with
			| TMono tref ->
				(Obj.magic tref : int) + 1
			| TEnum(e,tl) ->
				List.fold_left fold_hash ((Obj.magic e : int) + 3) tl
			| TInst(c,tl) ->
				List.fold_left fold_hash ((Obj.magic c : int) + 5) tl
			| TType(t,tl) ->
				List.fold_left fold_hash ((Obj.magic t : int) + 7) tl
			| TFun(a,ret) ->
				let cur = hash ret + 9 in
				List.fold_left (fun cur_hash (n,o,t) ->
					let h = Hashtbl.hash n in
					let h = if o then h + 1 else h + 2 in
					let h2 = hash t in
					let h_all = 31 * h + h2 in
					31 * cur_hash + h_all
				) cur a
			| TAnon(a) ->
				(Obj.magic a : int) + 11
			| TDynamic(t1) ->
				if t1 == t_dynamic then
					(Obj.magic t1 : int) + 13
				else
					hash t1 + 13
			| TAbstract(a,tl) ->
				List.fold_left fold_hash ((Obj.magic a : int) + 15) tl
			| TLazy _ -> assert false
end

module TypeHashtbl = Hashtbl.Make(TypeHash)

type context = {
	constants : (const_table, int) Hashtbl.t;
	constants_tvar : (int, int) Hashtbl.t;
	constants_type : int TypeHashtbl.t;
	mutable tcount : int;
	ctable : unit IO.output;
	ch : unit IO.output;
	mutable last_pos : pos;
}

let nstr ctx str =
	let tbl_data = TblString(str) in
	try
		Hashtbl.find ctx.constants tbl_data
	with | Not_found ->
		write_byte ctx.ctable (Char.code 's');
		write_ui16 ctx.ctable (String.length str);
		write_string ctx.ctable str;
		let count = ctx.tcount in
		ctx.tcount <- count + 1;
		Hashtbl.replace ctx.constants tbl_data count;
		count

let nmodule ctx (module_path, type_name) =
	let tbl_data = TblModuleRef((module_path,type_name)) in
	try
		Hashtbl.find ctx.constants tbl_data
	with | Not_found ->
		let pack, name = module_path in
		let npack = List.map (fun name ->
			nstr ctx name
		) pack in
		let nname = nstr ctx name in
		let ntype_name = nstr ctx type_name in
		write_byte ctx.ctable (Char.code 'm');
		List.iter (write_ui16 ctx.ctable) npack;
		write_ui16 ctx.ctable 0; (* tell we're done *)
		write_ui16 ctx.ctable nname;
		write_ui16 ctx.ctable ntype_name;
		let count = ctx.tcount in
		ctx.tcount <- count + 1;
		Hashtbl.replace ctx.constants tbl_data count;
		count

let write_module ctx buf m =
	write_ui16 buf (nmodule ctx m)

let get_real_path path meta =
	try
		match Meta.get Meta.RealPath meta with
		| (Meta.RealPath, [Ast.EConst (Ast.String (name)), _], _) ->
			parse_path name
		| _ -> raise Not_found
	with | Not_found ->
		path

(* no null tag *)
let write_pos ctx buf p =
	if p.pfile = ctx.last_pos.pfile && abs (p.pmin - ctx.last_pos.pmax) < 128 && abs (p.pmax - p.pmin) < 128 then begin
		write_byte buf (p.pmin - ctx.last_pos.pmax);
		write_byte buf (p.pmax - p.pmin)
	end else begin
		write_byte buf 0xFF;
		write_ui16 buf (nstr ctx p.pfile);
		write_i32 buf p.pmin;
		write_ui16 buf (p.pmax - p.pmin)
	end

(* no null tag *)
let write_enum_flags ctx buf enum_to_int flags =
	let flags = List.fold_left (fun acc e ->
		acc lor (1 lsl (enum_to_int e))) 0 flags
	in
	write_i32 buf flags

let access_code_flag acc = match acc with
	| APublic -> 0
	| APrivate -> 1
	| AStatic -> 2
	| AOverride -> 3
	| ADynamic -> 4
	| AInline -> 5
	| AMacro -> 6

let rec binop_code op = match op with
	| OpAdd -> 1
	| OpMult -> 2
	| OpDiv -> 3
	| OpSub -> 4
	| OpAssign -> 5
	| OpEq -> 6
	| OpNotEq -> 7
	| OpGte -> 8
	| OpLte -> 9
	| OpGt -> 10
	| OpLt -> 11
	| OpAnd -> 12
	| OpOr -> 13
	| OpXor -> 14
	| OpBoolAnd -> 15
	| OpBoolOr -> 16
	| OpShr -> 17
	| OpUShr -> 18
	| OpShl -> 19
	| OpMod -> 20
	| OpAssignOp op -> 22 + (binop_code op)
	| OpInterval -> 21
	| OpArrow -> 22

let unop_code op = match op with
	| Increment -> 1
	| Decrement -> 2
	| Not -> 3
	| Neg -> 4
	| NegBits -> 5

(* null tag: ui16 0 *)
let write_string ctx buf s =
	write_ui16 buf (nstr ctx s)

(* no null tag *)
let write_path ctx buf (pack,name) =
	List.iter (write_string ctx buf) pack;
	write_ui16 buf 0;
	write_string ctx buf name

(* no null tag *)
let write_doc ctx buf s = match s with
	| None -> write_ui16 buf 0
	| Some s -> write_string ctx buf s

(* null tag: ui16 0 (from write_string) *)
let write_placed_name ctx buf (s,p) =
	write_string ctx buf s;
	write_pos ctx buf p

(* null tag: ui8 0 *)
let write_module_type ctx buf mt = match mt with
	| TClassDecl c ->
		write_byte buf 1;
		write_module ctx buf (c.cl_module.m_path,snd (get_real_path c.cl_path c.cl_meta));
	| TEnumDecl e ->
		write_byte buf 2;
		write_module ctx buf (e.e_module.m_path,snd (get_real_path e.e_path e.e_meta));
	| TTypeDecl t ->
		write_byte buf 3;
		write_module ctx buf (t.t_module.m_path,snd (get_real_path t.t_path t.t_meta));
	| TAbstractDecl a ->
		write_byte buf 4;
		write_module ctx buf (a.a_module.m_path,snd (get_real_path a.a_path a.a_meta))

(* null tag: ui8 0 (from write_complex_type) *)
let rec write_type_hint ctx buf (ct,p) =
	write_complex_type ctx buf ct;
	write_pos ctx buf p

(* null tag: ui8 *)
and write_complex_type ctx buf ct =
	match ct with
		| CTPath tp ->
			write_byte buf 1;
			write_type_path ctx buf tp
		| CTFunction (thl,th) ->
			write_byte buf 2;
			write_type_hint ctx buf th;
			List.iter (write_type_hint ctx buf) thl;
			write_byte buf 0
		| CTAnonymous (cfl) ->
			write_byte buf 3;
			List.iter (write_class_field ctx buf) cfl;
			write_ui16 buf 0
		| CTParent th ->
			write_byte buf 4;
			write_type_hint ctx buf th
		| CTExtend (ptl,cfl) ->
			write_byte buf 5;
			List.iter (write_placed_type_path ctx buf) ptl;
			write_ui16 buf 0;
			List.iter (write_class_field ctx buf) cfl;
			write_ui16 buf 0
		| CTOptional th ->
			write_byte buf 6;
			write_type_hint ctx buf th

(* no null tag *)
and write_type_path ctx buf tp =
	List.iter (write_string ctx buf) tp.tpackage;
	write_ui16 buf 0;
	write_string ctx buf tp.tname;
	List.iter (function
		| TPType th ->
			write_byte buf 1;
			write_type_hint ctx buf th
		| TPExpr e ->
			write_byte buf 2;
			write_expr ctx buf e
	) tp.tparams;
	(match tp.tsub with
	| None -> write_ui16 buf 0
	| Some s -> write_string ctx buf s)

(* no null tag *)
and write_placed_type_path ctx buf (tp,p) =
	write_type_path ctx buf tp;
	write_pos ctx buf p

(* null tag: ui16 0 (from write_placed_name) *)
and write_type_param ctx buf tp =
	write_placed_name ctx buf tp.tp_name;
	List.iter (write_type_param ctx buf) tp.tp_params;
	write_byte buf 0;
	List.iter (write_type_hint ctx buf) tp.tp_constraints;
	write_byte buf 0;
	write_meta ctx buf tp.tp_meta

(* no null tag *)
and write_func ctx buf func =
	List.iter (write_type_param ctx buf) func.f_params;
	write_ui16 buf 0;
	List.iter (fun (pname,opt,meta,th_opt,eopt) ->
		write_placed_name ctx buf pname;
		write_byte buf (if opt then 1 else 0);
		write_meta ctx buf meta;
		(match th_opt with
		| None -> write_byte buf 0
		| Some th -> write_type_hint ctx buf th);
		(match eopt with
		| None -> write_byte buf 0
		| Some e -> write_expr ctx buf e)
	) func.f_args;
	write_ui16 buf 0;
	(match func.f_type with
	| None -> write_byte buf 0
	| Some th -> write_type_hint ctx buf th);
	(match func.f_expr with
	| None -> write_byte buf 0
	| Some e -> write_expr ctx buf e)

(* null tag: ui16 0 (from write_placed_name) *)
and write_class_field ctx buf cf =
	write_placed_name ctx buf cf.cff_name;
	write_doc ctx buf cf.cff_doc;
	write_pos ctx buf cf.cff_pos;
	write_meta ctx buf cf.cff_meta;
	write_enum_flags ctx buf access_code_flag cf.cff_access;
	match cf.cff_kind with
		| FVar(th_opt, eopt) ->
			write_byte buf 1;
			(match th_opt with
			| None -> write_byte buf 0
			| Some th -> write_type_hint ctx buf th);
			(match eopt with
			| None -> write_byte buf 0
			| Some e -> write_expr ctx buf e)
		| FFun fn ->
			write_byte buf 2;
			write_func ctx buf fn
		| FProp(read,write,th_opt,eopt) ->
			write_byte buf 3;
			write_placed_name ctx buf read;
			write_placed_name ctx buf write;
			(match th_opt with
			| None -> write_byte buf 0
			| Some th -> write_type_hint ctx buf th);
			(match eopt with
			| None -> write_byte buf 0
			| Some e -> write_expr ctx buf e)

(* null tag: ui8 0 (from write_expr_data) *)
and write_expr ctx buf (e,p) =
	write_expr_data ctx buf e;
	write_pos ctx buf p

(* null tag: ui8 0 *)
and write_expr_data ctx buf e =
	match e with
	| EConst c ->
		write_byte buf 1;
		(match c with
			| Regexp (r,f) ->
				write_byte buf 5;
				write_ui16 buf (nstr ctx r);
				write_ui16 buf (nstr ctx f)
			| Int s | Float s | String s | Ident s ->
				let b = match c with
					| Int _ -> 1 | Float _ -> 2
					| String _ -> 3 | Ident _ -> 4
					| _ -> assert false
				in
				write_byte buf b;
				write_ui16 buf (nstr ctx s))
	| EArray (e1, e2) ->
		write_byte buf 2;
		write_expr ctx buf e1;
		write_expr ctx buf e2
	| EBinop (op, e1, e2) ->
		write_byte buf 3;
		write_byte buf (binop_code op);
		write_expr ctx buf e1;
		write_expr ctx buf e2
	| EField (e1, field) ->
		write_byte buf 4;
		write_expr ctx buf e1;
		write_ui16 buf (nstr ctx field)
	| EParenthesis e1 ->
		write_byte buf 5;
		write_expr ctx buf e1
	| EObjectDecl nel ->
		write_byte buf 6;
		List.iter (fun (pname, expr) ->
			write_placed_name ctx buf pname;
			write_expr ctx buf expr
		) nel;
		write_ui16 buf 0
	| EArrayDecl el ->
		write_byte buf 7;
		List.iter (write_expr ctx buf) el;
		write_byte buf 0
	| ECall(e, el) ->
		write_byte buf 8;
		write_expr ctx buf e;
		List.iter (write_expr ctx buf) el;
		write_byte buf 0
	| ENew(ptp, el) ->
		write_byte buf 9;
		write_placed_type_path ctx buf ptp;
		List.iter (write_expr ctx buf) el;
		write_byte buf 0
	| EUnop(unop, unop_flag, expr) ->
		write_byte buf 10;
		write_byte buf (unop_code unop);
		write_byte buf (if unop_flag = Prefix then 1 else 2);
		write_expr ctx buf expr
	| EVars(vl) ->
		write_byte buf 11;
		List.iter(fun (pname, th_opt, eopt) ->
			write_placed_name ctx buf pname;
			(match th_opt with
			| None -> write_byte buf 0
			| Some th -> write_type_hint ctx buf th);
			(match eopt with
			| None -> write_byte buf 0
			| Some e -> write_expr ctx buf e)) vl;
		write_ui16 buf 0
	| EFunction(name_opt, func) ->
		write_byte buf 12;
		(match name_opt with
		| None -> write_ui16 buf 0
		| Some name -> write_string ctx buf name);
		write_func ctx buf func
	| EBlock el ->
		write_byte buf 13;
		List.iter (write_expr ctx buf) el;
		write_byte buf 0
	| EFor(e1,e2) ->
		write_byte buf 14;
		write_expr ctx buf e1;
		write_expr ctx buf e2
	| EIn(e1,e2) ->
		write_byte buf 15;
		write_expr ctx buf e1;
		write_expr ctx buf e2
	| EIf(e1,e2,e3_opt) ->
		write_byte buf 16;
		write_expr ctx buf e1;
		write_expr ctx buf e2;
		(match e3_opt with
		| None -> write_byte buf 0
		| Some e3 -> write_expr ctx buf e2)
	| EWhile(e1,e2,flag) ->
		write_byte buf 17;
		write_expr ctx buf e1;
		write_expr ctx buf e2;
		write_byte buf (if flag = NormalWhile then 1 else 2)
	| ESwitch(e1, cases, def) ->
		write_byte buf 18;
		write_expr ctx buf e1;
		write_ui16 buf (List.length cases); (* we cannot use null tags here, since there can be a valid 0 tag *)
		List.iter (fun (el, eopt, e2opt, p) ->
			List.iter (write_expr ctx buf) el;
			write_byte buf 0;
			(match eopt with
			| None -> write_byte buf 0
			| Some e -> write_expr ctx buf e);
			(match e2opt with
			| None -> write_byte buf 0
			| Some e -> write_expr ctx buf e);
			write_pos ctx buf p
		) cases;
		(match def with
		| None -> write_byte buf 0
		| Some (eopt, p) ->
			write_byte buf 1; (* we cannot use null tags here *)
			(match eopt with
			| None -> write_byte buf 0
			| Some e -> write_expr ctx buf e);
			write_pos ctx buf p)
	| ETry(e1,catches) ->
		write_byte buf 19;
		write_expr ctx buf e1;
		List.iter (fun (pn, th, e, p) ->
			write_placed_name ctx buf pn;
			write_type_hint ctx buf th;
			write_expr ctx buf e;
			write_pos ctx buf p
		) catches;
		write_ui16 buf 0
	| EReturn eopt ->
		write_byte buf 20;
		(match eopt with
		| None -> write_byte buf 0
		| Some e -> write_expr ctx buf e)
	| EBreak ->
		write_byte buf 21;
	| EContinue ->
		write_byte buf 22;
	| EUntyped e ->
		write_byte buf 23;
		write_expr ctx buf e
	| EThrow e ->
		write_byte buf 24;
		write_expr ctx buf e
	| ECast(e, th_opt) ->
		write_byte buf 25;
		write_expr ctx buf e;
		(match th_opt with
		| None -> write_byte buf 0
		| Some th -> write_type_hint ctx buf th)
	| EDisplay(e,b) ->
		write_byte buf 26;
		write_expr ctx buf e;
		write_byte buf (if b then 1 else 0)
	| EDisplayNew(ptp) ->
		write_byte buf 27;
		write_placed_type_path ctx buf ptp
	| ETernary(e1,e2,e3) ->
		write_byte buf 28;
		write_expr ctx buf e1;
		write_expr ctx buf e2;
		write_expr ctx buf e3
	| ECheckType(e,th) ->
		write_byte buf 29;
		write_expr ctx buf e;
		write_type_hint ctx buf th
	| EMeta(meta, e) ->
		write_meta_entry ctx buf meta;
		write_expr ctx buf e

(* null tag: ui16 0 (from write_string) *)
and write_meta_entry ctx buf (m,el,p) =
	write_string ctx buf (Meta.to_string m);
	List.iter (write_expr ctx buf) el;
	write_byte buf 0;
	write_pos ctx buf p

(* no null tag *)
and write_meta ctx buf meta =
	List.iter (write_meta_entry ctx buf) meta;
	write_ui16 buf 0

(* null tag: ui8 0 *)
let rec write_inplace_t ctx buf t =
	let t = normalize_type t in
	match t with
	| TMono(ref) ->
		write_byte buf (Char.code 'M')
	| TDynamic(d) ->
		if t == t_dynamic then
			write_byte buf (Char.code 'D')
		else begin
			write_byte buf (Char.code 'd');
			write_t ctx buf d
		end
	| TEnum(e,params) ->
		write_byte buf (Char.code 'E');
		write_module ctx buf (e.e_module.m_path,snd (get_real_path e.e_path e.e_meta));
		List.iter (fun p ->
			write_t ctx buf p
		) params;
		write_byte buf 0
	| TInst(c,params) -> (match c.cl_path with
		| [], "String" ->
			write_byte buf (Char.code 'S');
		| [], "Void" ->
			write_byte buf (Char.code 'V');
		| [], "Int" ->
			write_byte buf (Char.code 'I');
		| [], "Float" ->
			write_byte buf (Char.code 'F');
		| [], "Bool" ->
			write_byte buf (Char.code 'B');
		| [], "Array" ->
			write_byte buf (Char.code 'A');
			write_t ctx buf (List.hd params)
		| [], "Null" ->
			write_byte buf (Char.code 'N');
			write_t ctx buf (List.hd params)
		| _ -> (match c.cl_kind with
			| KTypeParameter tl ->
				write_byte buf (Char.code 'P');
				write_path ctx buf c.cl_path;
				List.iter (write_t ctx buf) tl;
				write_byte buf 0
			| KExpr e ->
				write_byte buf (Char.code 'e');
				write_path ctx buf c.cl_path;
				write_expr ctx buf e
			| _ ->
				write_byte buf (Char.code 'C');
				write_module ctx buf (c.cl_module.m_path,snd (get_real_path c.cl_path c.cl_meta));
				List.iter (fun p ->
					write_t ctx buf p
				) params;
				write_byte buf 0))
	| TType(t,params) ->
		write_byte buf (Char.code 'T');
		write_module ctx buf (t.t_module.m_path, snd (get_real_path t.t_path t.t_meta));
		List.iter (fun p ->
			write_t ctx buf p
		) params;
		write_byte buf 0
	| TFun(args, ret) ->
		write_byte buf (Char.code 'f');
		write_t ctx buf ret;
		List.iter (fun (n,o,t) ->
			write_ui16 buf (nstr ctx n);
			write_byte buf (if o then 1 else 0);
			write_t ctx buf t
		) args;
		write_byte buf 0
	| TAnon(anon) ->
		write_byte buf (Char.code 'a');
		let write_fields = match !(anon.a_status) with
			| Closed ->
				write_byte buf 1;
				true
			| Opened ->
				write_byte buf 2;
				true
			| Const ->
				write_byte buf 3;
				true
			| Extend tl ->
				write_byte buf 4;
				true
			| Statics c ->
				write_byte buf 5;
				write_module ctx buf (c.cl_module.m_path, snd (get_real_path c.cl_path c.cl_meta));
				false
			| EnumStatics e ->
				write_byte buf 6;
				write_module ctx buf (e.e_module.m_path, snd (get_real_path e.e_path e.e_meta));
				false
			| AbstractStatics a ->
				write_byte buf 7;
				write_module ctx buf (a.a_module.m_path, snd (get_real_path a.a_path a.a_meta));
				false
		in
		if write_fields then begin
			PMap.iter (fun _ field -> ()) anon.a_fields
		end;
		write_byte buf 0
	| TAbstract(a,params) ->
		write_byte buf (Char.code 'R');
		write_module ctx buf (a.a_module.m_path, snd (get_real_path a.a_path a.a_meta));
		List.iter (fun p ->
			write_t ctx buf p
		) params;
		write_byte buf 0
	| TLazy _ -> assert false

(* null tag: ui8 0 *)
and write_t ctx buf t =
	write_ui16 buf (ntype ctx t)

and ntype ctx t =
	let normalized = normalize_type t in
	try
		TypeHashtbl.find ctx.constants_type normalized
	with | Not_found ->
		let buf = IO.output_string() in
		write_inplace_t ctx buf t;
		IO.write_string ctx.ctable (close_out buf);
		let count = ctx.tcount in
		ctx.tcount <- count + 1;
		TypeHashtbl.replace ctx.constants_type normalized count;
		count

(* null tag: ui8 0 *)
let write_tconstant ctx buf = function
	| TInt i32 ->
		write_byte buf 1;
		write_real_i32 buf i32
	| TFloat f ->
		write_byte buf 2;
		write_string ctx buf f
	| TString s ->
		write_byte buf 3;
		write_string ctx buf s
	| TBool b ->
		let i = 4 in
		let i = if b then i land 0x80 else i in
		write_byte buf i
	| TNull ->
		write_byte buf 4
	| TThis ->
		write_byte buf 5
	| TSuper ->
		write_byte buf 6

(* null tag: ui8 0 *)
let rec write_texpr ctx buf e =
	write_texpr_expr ctx buf e.eexpr;
	write_t ctx buf e.etype;
	write_pos ctx buf e.epos

(* null tag: ui8 0 *)
and write_tfield_access ctx buf = function
	| FInstance(c,tl,field) ->
		write_byte buf 1;
		write_t ctx buf (TInst(c,tl));
		write_string ctx buf field.cf_name;
		write_type_params ctx buf field.cf_params;
		write_t ctx buf field.cf_type
	| FStatic(c,field) ->
		write_byte buf 2;
		write_module ctx buf (c.cl_module.m_path,snd (get_real_path c.cl_path c.cl_meta));
		write_string ctx buf field.cf_name;
		write_type_params ctx buf field.cf_params;
		write_t ctx buf field.cf_type
	| FAnon(field) ->
		write_byte buf 3;
		write_string ctx buf field.cf_name;
		write_type_params ctx buf field.cf_params;
		write_t ctx buf field.cf_type
	| FDynamic(name) ->
		write_byte buf 4;
		write_string ctx buf name
	| FClosure(copt, field) ->
		write_byte buf 5;
		(match copt with
		| None -> write_ui16 buf 0
		| Some(c,tl) -> write_t ctx buf (TInst(c,tl)));
		write_string ctx buf field.cf_name;
		write_t ctx buf field.cf_type
	| FEnum(e,ef) ->
		write_byte buf 6;
		write_module ctx buf (e.e_module.m_path,snd (get_real_path e.e_path e.e_meta));
		write_string ctx buf ef.ef_name

(* null tag: ui8 0 *)
and write_texpr_expr ctx buf e =
	match e with
	| TConst c ->
		write_byte buf 1;
		write_tconstant ctx buf c
	| TLocal v ->
		write_byte buf 2;
		write_var ctx buf v
	| TArray(e1, e2) ->
		write_byte buf 3;
		write_texpr ctx buf e1;
		write_texpr ctx buf e2
	| TBinop(op, e1, e2) ->
		write_byte buf 4;
		write_byte buf (binop_code op);
		write_texpr ctx buf e1;
		write_texpr ctx buf e2
	| TField(e, acc) ->
		write_byte buf 5;
		write_texpr ctx buf e;
		write_tfield_access ctx buf acc
	| TTypeExpr mt ->
		write_byte buf 6;
		write_module_type ctx buf mt
	| TParenthesis e ->
		write_byte buf 7;
		write_texpr ctx buf e
	| TObjectDecl (sel) ->
		write_byte buf 8;
		List.iter (fun (s,e) ->
			write_string ctx buf s;
			write_texpr ctx buf e
		) sel;
		write_ui16 buf 0
	| TArrayDecl el ->
		write_byte buf 9;
		List.iter (write_texpr ctx buf) el;
		write_byte buf 0
	| TCall(e,el) ->
		write_byte buf 10;
		write_texpr ctx buf e;
		List.iter (write_texpr ctx buf) el;
		write_byte buf 0
	| TNew (c,tl,el) ->
		write_byte buf 11;
		write_t ctx buf (TInst(c,tl));
		List.iter (write_texpr ctx buf) el;
		write_byte buf 0
	| TUnop(unop,flag,e) ->
		write_byte buf 12;
		write_byte buf (unop_code unop);
		write_byte buf (if flag = Prefix then 1 else 2);
		write_texpr ctx buf e
	| TFunction tf ->
		write_byte buf 13;
		write_tfunc ctx buf tf
	| TVar(tvar,eopt) ->
		write_byte buf 14;
		write_var ctx buf tvar;
		(match eopt with
		| None -> write_byte buf 0
		| Some e -> write_texpr ctx buf e)
	| TBlock el ->
		write_byte buf 15;
		List.iter (write_texpr ctx buf) el;
		write_byte buf 0
	| TFor(tvar, e1, e2) ->
		write_byte buf 16;
		write_var ctx buf tvar;
		write_texpr ctx buf e1;
		write_texpr ctx buf e2
	| TIf(econd, eif, eelse) ->
		write_byte buf 17;
		write_texpr ctx buf econd;
		write_texpr ctx buf eif;
		(match eelse with
		| None -> write_byte buf 0
		| Some e -> write_texpr ctx buf e)
	| TWhile(econd, e1, flag) ->
		write_byte buf 18;
		write_texpr ctx buf econd;
		write_texpr ctx buf e1;
		write_byte buf (if flag = NormalWhile then 1 else 2)
	| TSwitch(econd, matches, edef) ->
		write_byte buf 19;
		write_texpr ctx buf econd;
		write_ui16 buf (List.length matches);
		List.iter (fun (el, e) ->
			List.iter (write_texpr ctx buf) el;
			write_byte buf 0;
			write_texpr ctx buf e
		) matches;
		(match edef with
		| None -> write_byte buf 0
		| Some e -> write_texpr ctx buf e)
	| TTry(e1, catches) ->
		write_byte buf 20;
		write_texpr ctx buf e1;
		List.iter (fun (tvar,e) ->
			write_var ctx buf tvar;
			write_texpr ctx buf e
		) catches;
		write_ui16 buf 0
	| TReturn eopt ->
		write_byte buf 21;
		(match eopt with
		| None -> write_byte buf 0
		| Some e -> write_texpr ctx buf e)
	| TBreak ->
		write_byte buf 22;
	| TContinue ->
		write_byte buf 23;
	| TThrow e ->
		write_byte buf 24;
		write_texpr ctx buf e
	| TCast(e, mtopt) ->
		write_byte buf 25;
		(match mtopt with
		| None ->
			write_byte buf 0
		| Some mt ->
			write_module_type ctx buf mt)
	| TMeta(meta, e) ->
		write_byte buf 26;
		write_meta_entry ctx buf meta;
		write_texpr ctx buf e
	| TEnumParameter(e, ef, nparam) ->
		write_byte buf 27;
		write_texpr ctx buf e;
		write_string ctx buf ef.ef_name;
		write_ui16 buf nparam

and write_tfunc ctx buf tfunc =
	List.iter (fun (v,copt) ->
		write_var ctx buf v;
		match copt with
		| None -> write_byte buf 0
		| Some constant -> write_tconstant ctx buf constant) tfunc.tf_args;
	write_byte buf 0;
	write_t ctx buf tfunc.tf_type;
	write_texpr ctx buf tfunc.tf_expr

(* no null tag *)
and write_type_params ctx buf tparams =
	List.iter (fun (s,t) ->
		write_string ctx buf s;
		write_t ctx buf t) tparams;
	write_ui16 buf 0

and nvar ctx tvar =
	try
		Hashtbl.find ctx.constants_tvar tvar.v_id
	with | Not_found ->
		let buf = IO.output_string() in
		write_string ctx buf tvar.v_name;
		write_t ctx buf tvar.v_type;
		let flags = if tvar.v_capture then 1 else 0 in
		let flags = match tvar.v_extra with
			| None -> flags
			| Some _ -> flags land 0x2
		in
		write_byte buf flags;
		(match tvar.v_extra with
		| None -> ()
		| Some (tparams, topt) ->
			write_type_params ctx buf tparams;
			(match topt with
			| None -> write_byte buf 0
			| Some e -> write_texpr ctx buf e));
		write_meta ctx buf tvar.v_meta;
		write_pos ctx buf tvar.v_pos;
		IO.write_string ctx.ctable (close_out buf);
		let count = ctx.tcount in
		ctx.tcount <- count + 1;
		Hashtbl.replace ctx.constants_tvar tvar.v_id count;
		count

(* null tag: ui16 0 *)
and write_var ctx buf tvar =
	write_ui16 buf (nvar ctx tvar)

let write_enum_field ctx buf ef =
	write_string ctx buf ef.ef_name;
	write_t ctx buf ef.ef_type;
	write_pos ctx buf ef.ef_pos;
	write_pos ctx buf ef.ef_name_pos;
	write_doc ctx buf ef.ef_doc;
	write_i32 buf ef.ef_index;
	write_type_params ctx buf ef.ef_params;
	write_meta ctx buf ef.ef_meta

let write_enum ctx buf e =
	write_byte buf (Char.code '*');
	write_byte buf (Char.code 'E');
	let (pack, name) = get_real_path e.e_path e.e_meta in
	write_module ctx buf (e.e_module.m_path,name);
	write_pos ctx buf e.e_pos;
	write_pos ctx buf e.e_name_pos;
	let flags = if e.e_private then 1 else 0 in
	let flags = if e.e_extern then flags land 0x2 else flags in
	write_doc ctx buf e.e_doc;
	write_meta ctx buf e.e_meta;
	write_type_params ctx buf e.e_params;
	write_ui16 buf (List.length e.e_names);
	List.iter (fun name ->
		let constr = PMap.find name e.e_constrs in
		write_enum_field ctx buf constr) e.e_names;
	write_byte buf (Char.code '$');

let write_tdef ctx buf tdef =
	write_byte buf (Char.code '*');
	write_byte buf (Char.code 'T');
	let (pack, name) = get_real_path tdef.t_path tdef.t_meta in
	write_pos ctx buf tdef.t_pos;
	write_pos ctx buf tdef.t_name_pos;
	write_byte buf (if tdef.t_private then 1 else 0);
	write_doc ctx buf tdef.t_doc;
	write_meta ctx buf tdef.t_meta;
	write_type_params ctx buf tdef.t_params;
	write_t ctx buf tdef.t_type;
	write_byte buf (Char.code '$');

let write_tabstract ctx buf a =
	let write_field_ref cf =
		write_string ctx buf cf.cf_name;
		write_type_params ctx buf cf.cf_params;
		write_t ctx buf cf.cf_type
	in
	write_byte buf (Char.code '*');
	write_byte buf (Char.code 'A');
	let (pack, name) = get_real_path a.a_path a.a_meta in
	write_pos ctx buf a.a_pos;
	write_pos ctx buf a.a_name_pos;
	write_byte buf (if a.a_private then 1 else 0);
	write_doc ctx buf a.a_doc;
	write_meta ctx buf a.a_meta;
	write_type_params ctx buf a.a_params;
	List.iter (fun (binop,cf) ->
		write_byte buf (binop_code binop);
		write_field_ref cf
	) a.a_ops;
	write_byte buf 0;
	List.iter (fun (unop,cf) ->
		write_byte buf (unop_code binop);
		write_field_ref cf
	) a.a_unops;
	write_byte buf 0;
	(match a.a_impl with
	| None -> write_ui16 buf 0
	| Some c ->
		write_module ctx buf (c.cl_module.m_path,snd (get_real_path c.cl_path c.cl_meta)));
	write_t ctx buf a.a_this;
	List.iter (write_t ctx buf) a.a_from;
	write_ui16 buf 0;
	List.iter (fun (t,cf) ->
		write_t ctx buf t;
		write_field_ref cf) a.a_from_field;
	write_ui16 buf 0;
	List.iter (write_t ctx buf) a.a_to;
	write_ui16 buf 0;
	List.iter (fun (t,cf) ->
		write_t ctx buf t;
		write_field_ref cf) a.a_to_field;
	write_ui16 buf 0;
	List.iter (write_field_ref) a.a_array;
	write_ui16 buf 0;
	(match a.a_resolve with
	| None -> write_ui16 buf 0
	| Some cf -> write_field_ref cf);
	write_byte buf (Char.code '$')

(* and tclass = { *)
(* 	mutable cl_path : path; *)
(* 	mutable cl_module : module_def; *)
(* 	mutable cl_pos : pos; *)
(* 	mutable cl_name_pos : pos; *)
(* 	mutable cl_private : bool; *)
(* 	mutable cl_doc : Ast.documentation; *)
(* 	mutable cl_meta : metadata; *)
(* 	mutable cl_params : type_params; *)
(* 	(* do not insert any fields above *) *)
(* 	mutable cl_kind : tclass_kind; *)
(* 	mutable cl_extern : bool; *)
(* 	mutable cl_interface : bool; *)
(* 	mutable cl_super : (tclass * tparams) option; *)
(* 	mutable cl_implements : (tclass * tparams) list; *)
(* 	mutable cl_fields : (string , tclass_field) PMap.t; *)
(* 	mutable cl_statics : (string, tclass_field) PMap.t; *)
(* 	mutable cl_ordered_statics : tclass_field list; *)
(* 	mutable cl_ordered_fields : tclass_field list; *)
(* 	mutable cl_dynamic : t option; *)
(* 	mutable cl_array_access : t option; *)
(* 	mutable cl_constructor : tclass_field option; *)
(* 	mutable cl_init : texpr option; *)
(* 	mutable cl_overrides : tclass_field list; *)
(*  *)
(* 	mutable cl_build : unit -> build_state; *)
(* 	mutable cl_restore : unit -> unit; *)
(* } *)
(* and tclass_field = { *)
(* 	mutable cf_name : string; *)
(* 	mutable cf_type : t; *)
(* 	mutable cf_public : bool; *)
(* 	cf_pos : pos; *)
(* 	cf_name_pos : pos; *)
(* 	mutable cf_doc : Ast.documentation; *)
(* 	mutable cf_meta : metadata; *)
(* 	mutable cf_kind : field_kind; *)
(* 	mutable cf_params : type_params; *)
(* 	mutable cf_expr : texpr option; *)
(* 	mutable cf_expr_unoptimized : tfunc option; *)
(* 	mutable cf_overloads : tclass_field list; *)
(* } *)

let write_class_field ctx buf is_override cf =
	write_string ctx buf cf.cf_name;
	write_t ctx buf cf.cf_type;
	let flags = if cf.cf_public then 0x1 else 0x0 in
	let flags = if is_override then flags land 0x2 else flags in
	write_byte buf flags;
	write_pos ctx buf cf.cf_pos;
	write_pos ctx buf cf.cf_name_pos;
	write_doc ctx buf cf.cf_doc;
	write_meta ctx buf cf.cf_meta;
	write_type_params ctx buf cf.cf_params;
	(match cf.cf_expr with
	| None -> write_ui16 buf 0
	| Some e ->
		let exprbuf = IO.output_string() in
		write_texpr ctx exprbuf e;
		(match cf.cf_expr_unoptimized with
		| None -> write_ui16 exprbuf 0
		| Some tf -> write_tfunc ctx exprbuf tf);
		let str = close_out buf;
		write_ui16 buf (String.length str);
		IO.write_string buf str)


let write_tclass ctx buf c =
	write_byte buf (Char.code '*');
	write_byte buf (Char.code 'C');
	let (pack, name) = get_real_path c.cl_path c.cl_meta in
	write_pos ctx buf c.cl_pos;
	write_pos ctx buf c.cl_name_pos;
	write_byte buf (if c.cl_private then 1 else 0);
	write_doc ctx buf c.cl_doc;
	write_meta ctx buf c.cl_meta;
	write_type_params ctx buf c.cl_params;
	(match c.cl_kind with
	| KNormal -< write_byte buf 1
	| KGeneric -> write_byte buf 2
	| KGenericInstance(c,tl) ->
			write_byte buf 3;
			write_t ctx buf (TInst(c,tl))
	| KMacroType ->
			write_byte buf 4
	| KGenericBuild(cfl) ->
			write_byte buf 5;
	| KAbstractImpl a ->
	| KTypeParameter _ | KExpr _ -> assert false);
	()
