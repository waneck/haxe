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
		Hashtbl.add ctx.constants tbl_data count;
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
		Hashtbl.add ctx.constants tbl_data count;
		count

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

(* type type_path = { *)
(* 	tpackage : string list; *)
(* 	tname : string; *)
(* 	tparams : type_param_or_const list; *)
(* 	tsub : string option; *)
(* } *)

(* null tag: ui16 0 *)
let write_string ctx buf s =
	write_ui16 buf (nstr ctx s)

(* no null tag *)
let write_doc ctx buf s = match s with
	| None -> write_ui16 buf 0
	| Some s -> write_string ctx buf s

(* null tag: ui16 0 (from write_string) *)
let write_placed_name ctx buf (s,p) =
	write_string ctx buf s;
	write_pos ctx buf p

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
let rec write_t ctx buf t =
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
		write_ui16 buf (nmodule ctx (e.e_module.m_path,snd e.e_path));
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
		| _ ->
			write_byte buf (Char.code 'C');
			write_ui16 buf (nmodule ctx (c.cl_module.m_path,snd c.cl_path));
			List.iter (fun p ->
				write_t ctx buf p
			) params;
			write_byte buf 0)
	| TType(t,params) ->
		write_byte buf (Char.code 'T');
		write_ui16 buf (nmodule ctx (t.t_module.m_path, snd t.t_path));
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
				write_ui16 buf (nmodule ctx (c.cl_module.m_path, snd c.cl_path));
				false
			| EnumStatics e ->
				write_byte buf 6;
				write_ui16 buf (nmodule ctx (e.e_module.m_path, snd e.e_path));
				false
			| AbstractStatics a ->
				write_byte buf 7;
				write_ui16 buf (nmodule ctx (a.a_module.m_path, snd a.a_path));
				false
		in
		if write_fields then begin
			PMap.iter (fun _ field -> ()) anon.a_fields
		end;
		write_byte buf 0
	| TAbstract(a,params) ->
		write_byte buf (Char.code 'R');
		write_ui16 buf (nmodule ctx (a.a_module.m_path, snd a.a_path));
		List.iter (fun p ->
			write_t ctx buf p
		) params;
		write_byte buf 0

(* let rec ntype ctx t = *)
(* 	let buf = IO.output_string() in *)
(* 	let t = normalize_type t in *)
(* 	() *)

(* let nvar ctx tvar = *)
(* 	try *)
(* 		Hashtbl.find ctx.constants_tvar tvar.v_id *)
(* 	with | Not_found -> *)
(* 		let nname = nstr ctx tvar.v_name in *)
(* 		let ntype =  *)

	(* | EParenthesis of expr *)
	(* | EObjectDecl of (placed_name * expr) list *)
	(* | EArrayDecl of expr list *)
	(* | ECall of expr * expr list *)
	(* | ENew of placed_type_path * expr list *)
	(* | EUnop of unop * unop_flag * expr *)
	(* | EVars of (placed_name * type_hint option * expr option) list *)
	(* | EFunction of string option * func *)
	(* | EBlock of expr list *)
	(* | EFor of expr * expr *)
	(* | EIn of expr * expr *)
	(* | EIf of expr * expr * expr option *)
	(* | EWhile of expr * expr * while_flag *)
	(* | ESwitch of expr * (expr list * expr option * expr option * pos) list * (expr option * pos) option *)
	(* | ETry of expr * (placed_name * type_hint * expr * pos) list *)
	(* | EReturn of expr option *)
	(* | EBreak *)
	(* | EContinue *)
	(* | EUntyped of expr *)
	(* | EThrow of expr *)
	(* | ECast of expr * type_hint option *)
	(* | EDisplay of expr * bool *)
	(* | EDisplayNew of placed_type_path *)
	(* | ETernary of expr * expr * expr *)
	(* | ECheckType of expr * type_hint *)
	(* | EMeta of metadata_entry * expr *)
