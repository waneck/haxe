(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)
open Flags;;
open Type;;
open Ast;;
open Common;;
open Gcrtypes;;
open Gcrtypes.Filters;;
open Gcrtypes.Expr;;
open Gcrtypes.Helpers;;

(** Common filters and c-ast helpers **)

(** Mapping texpr to c-ast **)
(****************************)

module TExprMap =
struct
(* in order to map texpr into c-ast, we need to implement the following abstract structures: *)

	type type_conversion = {
		(* how types are mapped into ctypes *)
		(* tc_apply_params : module_type -> (string * t) list -> tparams -> t -> t; *)
		map_params : (site_info -> tparams -> cparams) option;
			(* some targets will map their type parameters differently *)
			(* (e.g. Java and its lack of support for basic types in tparams) *)
		map_type : t -> ct;
		map_arg : (site_info -> (tvar * tconstant option) -> fun_arg) option;
	}

	and extern_loading = {
		(* try to convert target-specific externs into more accurate types *)
		(* (using e.g. information from a native lib) *)
		etry_load : module_type -> cls option;
	}

	and enum_conversion = {
		mutable enum_super : (cls * cparams) option;
		enum_to_cls : tenum -> cls;
		enum_field_acc : texpr -> tfield_access;
			(* texpr will be either a TField or a TCall( TField, _ ) *)
		enum_to_ct : t -> ct;
		enum_param_get : texpr -> tenum_field -> int -> expr;
	}

	and anon_conversion = {
		anon_to_ct : t -> ct;
			(* this call will make an effort to preserve typedefs *)
		anon_field_acc : texpr -> tfield_access;
			(* texpr will be either a TField or a TCall( TField, _ ) *)
		anon_objdecl : anon_site_info -> texpr -> expr;
			(* handle objdecl *)
	}

	and site_info =
		| ClassSite of module_type
		| FieldSite of module_type * tclass_field
		| EnumFieldSite of tenum * tenum_field
		| AnonFunctionSite of tfunc * type_params

	and anon_site_info =
		| AMeta of module_type
		| ASite of site_info

(* context *)
	and conv_ctx = {
		ccom : Common.context;
		cgen : gen;

		(* configurable behaviours *)
		mutable extern : extern_loading option;
		mutable econv : enum_conversion;
		mutable aconv : anon_conversion;
		mutable tconv : type_conversion;
		mutable legal_metas : (Meta.strict_meta, Ast.expr list -> intrinsic * ct) PMap.t;

		(* internal props *)
		mutable cur_class : tclass;
		mutable cur_field : tclass_field option;
		mutable class_types : type_params;
		mutable fun_stack : (func * type_params) list;
			(* stack of functions *)
		mutable var_map : (int, var) PMap.t;
		mutable infos : ( (path * int), cls_info ) PMap.t;
	}

	(** field declaration, types and overload cache **)
	and cls_info = {
		ci_path_name : path * int;
			(* type path and num of type params *)
		ci_key : module_type option;
			(* the haxe type associated with it *)
		ci_ref : cls;
			(* the class type being converted *)
		mutable ci_declared : declared_field list;
			(* all fields declared in this class *)
		mutable ci_cached_fields : (string, field_ref list) PMap.t;
			(* all fields available in this class : both inherited and declared *)
		mutable ci_supers : (cls_info * cparams) list; (* super and implements *)
	}

	and declared_field = {
		d_original : tclass_field;
		d_converted : field;
	}

	and field_ref = {
		r_field : field;
		r_type : ct;
		r_original : tclass_field option;
	}

	(** conversion **)
	(*****************)
	(* FIXME TODO IMPLEMENTME *)
	let cls_from_md ctx md = alloc_cls ~path:null_path ()

	let ct_from_md ctx md =
		let cls = cls_from_md ctx md in
		mktr (Inst(cls, mkcls_params cls))

	let map_params fn params =
		Array.map fn (Array.of_list params)

	let lookup_tparam ctx ctp =
		let idx lst = listfind_i (fun (_,t) -> match follow t with
			| TInst(c,_) -> c == ctp
			| _ -> false) lst
		in
		try
			TypeParam (idx ctx.class_types)
		with | Not_found ->
			let rec loop acc lst = match lst with
				| [] -> raise Not_found
				| (_, hd) :: tl -> try
					MethodTypeParam (acc, idx hd)
				with | Not_found ->
					loop (acc + 1) tl
			in
			loop 0 ctx.fun_stack

	let rec get_cparams ctx site tparams =
		if tparams = [] then
			empty_params
		else match ctx.tconv.map_params with
		| None ->
			Array.of_list (List.map (c_type ctx) tparams)
		| Some m ->
			m site tparams

	and c_field ctx original_t static cf =
		let name = cf.cf_name in
		let flags = if cf.cf_public then Flags.set Flags.empty FPublic else Flags.empty in
		let flags = if Meta.has Meta.Overload cf.cf_meta then Flags.set flags FOverload else flags in
		let vis = if cf.cf_public then VPublic else VPrivate in
		let ftype = c_type ctx original_t in
		let kind = match cf.cf_kind with
			| Var ({ v_read = AccCall } as v) | Var ({ v_write = AccCall } as v) ->
				if Type.is_extern_field cf then
					KProp (v.v_read = AccCall, v.v_write = AccCall)
				else
					KVarProp (v.v_read = AccCall, v.v_write = AccCall)
			| Var _ ->
				let rec get_const e = match e.eexpr with
					| TConst(c) -> Some (c_const ctx c)
					| TMeta(_,e) -> get_const e
					| TParenthesis(e) -> get_const e
					| _ -> None
				in
				let const = match cf.cf_expr with
					| None -> None
					| Some e -> get_const e
				in
				KVar const
			| Method fn ->
				KMethod None
		in
		alloc_field
			~static
			~name
			~ftype
			~kind
			~vis
			~flags
			()

	and get_cls_info ctx cl =
		let path = cl.cl_path, List.length cl.cl_params in
		try
			PMap.find path ctx.infos
		with Not_found ->
			let get_cp (c,p) =
				let cinfo = get_cls_info ctx c in
				let site = ClassSite (TClassDecl c) in
				cinfo,get_cparams ctx site p
			in
			let super = Option.map get_cp cl.cl_super in
			let ifaces = List.map get_cp cl.cl_implements in
			let supers = if cl.cl_interface then
				match super with
					| None -> []
					| Some (c,p) -> [c,p]
				else
					ifaces
			in
			(* if class is an interface, join all fields collected *)
			let cached = List.fold_left (fun acc (cinfo,param) ->
				let aparams = apply_cparams cinfo.ci_ref param in
				PMap.foldi (fun name allf acc ->
					let ret = List.map (fun fr ->
						{ fr with r_type = aparams fr.r_type }
					) allf in
					let ret = try
						let f = PMap.find name acc in
						f @ List.filter (fun f1 ->
							Flags.has f1.r_field.fflags FStatic ||
							not (List.exists (fun f2 -> is_override f1.r_field f2.r_field ) f)
						) ret
					with Not_found ->
						ret
					in
					PMap.add name ret acc
				) acc cinfo.ci_cached_fields
			) (PMap.empty) supers in

			let get_override f1 =
				let fl = PMap.find f1.fname cached in
				List.find (fun f2 -> same_overload_args f1 f2.r_field) fl
			in
			let cached = ref cached in
			let get_field static cf =
				let f = c_field ctx cf.cf_type static cf in
				let fl = try
					let o = get_override f in
					let fl = PMap.find f.fname !cached in
					f.ftype <- o.r_type;
					f.foverride <- Some o.r_field;
					let r = { r_field = f; r_type = o.r_type; r_original = Some cf } in
					List.map (fun f2 -> if f2.r_field == o.r_field then r else f2) fl
				with Not_found -> try
					let r = { r_field = f; r_type = f.ftype; r_original = Some cf } in
					let fl = PMap.find f.fname !cached in
					r :: fl
				with Not_found ->
					let r = { r_field = f; r_type = f.ftype; r_original = Some cf } in
					[r]
				in
				cached := PMap.add f.fname fl !cached;
				{ d_original = cf; d_converted = f }
			in
			let statics = List.map (get_field true) cl.cl_ordered_statics in
			let fields = statics @ List.map (get_field false) cl.cl_ordered_fields in
			let get_cp (i,p) = i.ci_ref,p in
			let cls = alloc_cls
				~path:cl.cl_path
				~types:(c_tparams ctx cl.cl_params)
				~super:(Option.map get_cp super)
				~implements:(List.map get_cp ifaces)
				~fields:(List.map (fun d -> d.d_converted) fields)
				()
			in
			let ret = {
				ci_path_name = path;
				ci_key = Some( TClassDecl cl );
				ci_ref = cls;
				ci_declared = statics @ fields;
				ci_cached_fields = !cached;
				ci_supers = supers;
			} in
			ctx.infos <- PMap.add path ret ctx.infos;
			ret

	and c_tparams ctx t =
		Array.of_list (List.map (fun (s,t) ->
			let constr = match follow t with
			| TInst({ cl_kind = KTypeParameter pl },_) -> List.map (c_type ctx) pl
			| _ -> []
			in
			{ pname = s; pconstraints = constr }
		) t)

	and c_type ctx = function
		| TMono r -> (match !r with
			| Some t -> c_type ctx t
			| _ -> mktr Dynamic)
		| TLazy f ->
			c_type ctx (!f())
		| TType ({ t_path = [],"Null" }, [t]) -> (match c_type ctx t with
			| { ctype = V(v) } -> mkt (Null v)
			| t -> t)
		| TType (t,tl) -> (match follow (TType(t,tl)) with
			| TAnon a ->
				ctx.aconv.anon_to_ct (TType(t,tl))
			| _ ->
				c_type ctx (apply_params t.t_params tl t.t_type))
		| TAnon a -> (match !(a.a_status) with
			| Statics c ->
				mktr (Type (ct_from_md ctx (TClassDecl c)) )
			| EnumStatics c ->
				mktr (Type (ct_from_md ctx (TEnumDecl c)) )
			| AbstractStatics a ->
				mktr (Type (ct_from_md ctx (TAbstractDecl a)) )
			| _ ->
				ctx.aconv.anon_to_ct (TAnon a))
		| TInst(({ cl_kind = KTypeParameter _ } as ctp),_) ->
			mkt ~&(lookup_tparam ctx ctp)
		| TInst({ cl_path = [],"Array" } as c, [t]) ->
			let ct = (get_cparams ctx (ClassSite (TClassDecl c)) [t]).(0) in
			mktr (Array ct)
		| TInst({ cl_path = [],"String" }, []) ->
			mktr String
		| TInst({ cl_path = _,"NativeArray" } as c, [t]) ->
			let site = ClassSite (TClassDecl c) in
			let ct = (get_cparams ctx site [t]).(0) in
			mktr (Vector ct)
		| TInst(c,p) when Meta.has Meta.Struct c.cl_meta ->
			let site = ClassSite (TClassDecl c) in
			mkt ~&(Struct(cls_from_md ctx (TClassDecl c), get_cparams ctx site p))
		| TInst(c,p) ->
			let site = ClassSite (TClassDecl c) in
			mktr (Inst(cls_from_md ctx (TClassDecl c), get_cparams ctx site p))
		| TEnum(e,p) ->
			ctx.econv.enum_to_ct (TEnum(e,p))
		| TFun(args,ret) ->
			(* when converting type from methods, take care to eliminate VarFunc *)
			mktr (Fun( [VarFunc], List.map (fun (n,o,t) ->
				c_type ctx t
			) args,c_type ctx ret ))
		| TAbstract(a,p) when Meta.has Meta.CoreType a.a_meta ->
			c_type ctx (Abstract.get_underlying_type a p)
		| TDynamic _ -> mktr Dynamic
		(* core type *)
		| TAbstract(a,p) -> mkt (try match a.a_path with
			| [],"Int" -> ~&(I32 true)
			| [],"UInt" -> ~&(I32 false)
			| [],"Float" -> ~&F64
			| [],"Single" -> ~&F32
			| [],"Void" -> ~&Void
			| [],"Bool" -> ~&Bool
			| ["haxe"],"Int64" -> ~&(I64 true)
			| _, "Int8" -> ~&(I8 true)
			| _, "UInt8" -> ~&(I8 false)
			| _, "Int16" -> ~&(I16 true)
			| _, "UInt16" -> ~&(I16 false)
			| _, "Int64" -> ~&(I64 true)
			| _, "UInt64" -> ~&(I64 false)
			| _, "IntPtr" -> ~&(IntPtr true)
			| _, "UIntPtr" -> ~&(IntPtr false)
			| _, "Char16" | _, "Char" -> ~&Char
			| _, "Pointer" ->
				let p = match p with
					| [p] -> p
					| _ -> raise Not_found
				in
				R(Pointer(c_type ctx p))
			| _ -> raise Not_found
		with | Not_found ->
			ctx.ccom.error ("Unknown core type: " ^ (path_s a.a_path)) a.a_pos;
			assert false)

	and c_const ctx = function
		| TInt i -> I i
		| TFloat f -> F f
		| TString s -> S s
		| TBool b -> B b
		| TNull -> Nil
		| TThis -> This
		| TSuper -> Super

	and c_var ctx v = try
		PMap.find v.v_id ctx.var_map
	with | Not_found ->
		let vconv = Gcrtypes.alloc_var
			~name:v.v_name
			~vtype:(c_type ctx v.v_type)
			~kind:VUndeclared (* Not_found here always means it's undeclared *)
			()
		in
		ctx.var_map <- PMap.add v.v_id vconv ctx.var_map;
		vconv

	and is_intrinsic name =
		String.length name > 2 &&
		String.get name 0 = '_' &&
		String.get name 1 = '_'

	and mk_fvar_access ctx field ct =
		{ a_field = field; a_overload = false; a_params = empty_params  }

	and clean_casts ctx expr = match expr.expr with
		| Cast(e,_) -> clean_casts ctx e
		| _ -> expr

	(* let find_first_declared_field gen orig_cl ?exact_field field = *)
	(* 	let chosen = ref None in *)
	(* 	let is_overload = ref false in *)
	(* 	let rec loop_cl depth c tl tlch = *)
	(* 		(try *)
	(* 			let ret = PMap.find field c.cl_fields in *)
	(* 			if Meta.has Meta.Overload ret.cf_meta then is_overload := true; *)
	(* 		match !chosen, exact_field with *)
	(* 		| Some(d,_,_,_,_), _ when depth <= d -> () *)
	(* 		| _, None -> *)
	(* 			chosen := Some(depth,ret,c,tl,tlch) *)
	(* 		| _, Some f2 -> *)
	(* 			List.iter (fun f -> *)
	(* 				let declared_t = apply_params c.cl_params tl f.cf_type in *)
	(* 				if Typeload.same_overload_args declared_t f2.cf_type f f2 then *)
	(* 					chosen := Some(depth,f,c,tl,tlch) *)
	(* 				) (ret :: ret.cf_overloads) *)
	(* 		with | Not_found -> ()); *)
	(* 	(match c.cl_super with *)
	(* 	| Some (sup,stl) -> *)
	(* 			let tl = List.map (apply_params c.cl_params tl) stl in *)
	(* 			let stl = gen.greal_type_param (TClassDecl sup) stl in *)
	(* 			let tlch = List.map (apply_params c.cl_params tlch) stl in *)
	(* 			loop_cl (depth+1) sup tl tlch *)
	(* 	| None -> ()); *)
	(* 	if c.cl_interface then *)
	(* 		List.iter (fun (sup,stl) -> *)
	(* 			let tl = List.map (apply_params c.cl_params tl) stl in *)
	(* 			let stl = gen.greal_type_param (TClassDecl sup) stl in *)
	(* 			let tlch = List.map (apply_params c.cl_params tlch) stl in *)
	(* 			loop_cl (depth+1) sup tl tlch *)
	(* 			) c.cl_implements *)
	(* 			in *)
	(* loop_cl 0 orig_cl (List.map snd orig_cl.cl_params) (List.map snd orig_cl.cl_params); *)
	(* match !chosen with *)
	(* | None -> None *)
	(* | Some(_,f,c,tl,tlch) -> *)
	(* 		if !is_overload && not (Meta.has Meta.Overload f.cf_meta) then *)
	(* 			f.cf_meta <- (Meta.Overload,[],f.cf_pos) :: f.cf_meta; *)
	(* 	let declared_t = apply_params c.cl_params tl f.cf_type in *)
	(* 	let params_t = apply_params c.cl_params tlch f.cf_type in *)
	(* 	let actual_t = match follow params_t with *)
	(* 	| TFun(args,ret) -> TFun(List.map (fun (n,o,t) -> (n,o,gen.greal_type t)) args, gen.greal_type ret) *)
	(* 	| _ -> gen.greal_type params_t in *)
	(* 	Some(f,actual_t,declared_t,params_t,c,tl,tlch) *)

	(* let find_first_declared_field ctx c fname = *)
		(* get the first declaration of a field, with its declared type. *)
		(* raises Not_found if field is not found *)
		(* let rec loop_cl *)

	and c_expr ctx ?stype e = match e.eexpr with
		| TConst c -> Const( c_const ctx c ) ++ (c_type ctx e.etype) @@ e.epos
		| TLocal v -> (try
			let vconv = try
					PMap.find v.v_id ctx.var_map
				with | Not_found when not (is_intrinsic v.v_name) -> (* check if intrinsic *)
					let vconv = Gcrtypes.alloc_var
						~name:v.v_name
						~vtype:(c_type ctx v.v_type)
						~kind:VUndeclared (* Not_found here always means it's undeclared *)
						()
					in
					ctx.var_map <- PMap.add v.v_id vconv ctx.var_map;
					vconv
				in
				(* Local will always have the same type as v.v_type *)
				Local vconv ++ vconv.vtype @@ e.epos
			with | Not_found ->
				let t = c_type ctx v.v_type in
				Intrinsic (get_intrinsic v.v_name t [], []) ++ t @@ e.epos)
		| TTypeExpr md ->
			let ct = ct_from_md ctx md in
			Const( Class ct ) +* Type ct @@ e.epos
		| TParenthesis e ->
			(* we completely ignore parenthesis *)
			c_expr ctx e
		| TObjectDecl _ ->
			let site = match ctx.cur_field with
				| None -> ASite (ClassSite( TClassDecl ctx.cur_class ))
				| Some f when f.cf_name = "__meta__" ->
						AMeta( TClassDecl ctx.cur_class ) (* FIXME: how to represent meta of enums? *)
				| Some f -> ASite(FieldSite( TClassDecl ctx.cur_class, f))
			in
			ctx.aconv.anon_objdecl site e
		| TArrayDecl decl ->
			let ct = c_type ctx e.etype in
			let base_t = match ct.ctype with
				| R(Array t) -> t
				| _ -> assert false
			in
			let convert e =
				cast_if_needed ctx base_t (c_expr ctx e)
			in
			Intrinsic( IArrayDecl(base_t) , List.map convert decl ) ++ ct @@ e.epos
		| TArray (e1,e2) ->
			let ex1 = c_expr ctx e1 in
			let ex2 = c_expr ctx e2 in
			let expected = c_type ctx e.etype in
			let t, acc = match ex1.t.ctype with
				| R(Pointer ct) | R(Array ct) | R(Vector ct) ->
					ct, ArrBuiltin
				| R(Inst(c,p)) | V(Struct(c,p))
				| Null(Struct(c,p)) -> (try
					let f = PMap.find "__array" c.cvars in
					let t = apply_cparams c p f.ftype in
					let acc = mk_fvar_access ctx f t in
					t, ArrClassField (acc)
				with Not_found ->
					mktr Dynamic, ArrNotFound)
				| _ -> mktr Dynamic, ArrDynamic (c_type ctx e.etype)
			in
			cast_if_needed ctx expected (ArrayAcc (ex1,ex2,acc) ++ t @@ e.epos)
		| TCast (e1,et) ->
			let t = c_type ctx e.etype in
			let ex1 = c_expr ctx e1 in
			Cast(ex1, if et = None then UnsafeCast else SafeCast) ++ t @@ e.epos
		| TMeta ((m,el,_),e1) ->
			(* there's a problem here if the TMeta is expected to be a statement *)
			let ex1 = c_expr ctx e1 in
			let ct = c_type ctx e.etype in
			(try
				let fn = PMap.find m ctx.legal_metas in
				let isic, ct = fn el in
				cast_if_needed ctx ct (Intrinsic (isic, [ex1]) ++ ct @@ e.epos)
			with | Not_found ->
				cast_if_needed ctx ct ex1)
		| TEnumParameter (e1,ef,i) ->
			ctx.econv.enum_param_get e1 ef i
		| TIf (econd,eif, Some(eelse)) ->
			let ct = c_type ctx e.etype in
			let excond = cast_if_needed ctx bool_t (c_expr ctx econd) in
			let ex1 = cast_if_needed ctx ct (c_expr ctx eif) in
			let ex2 = cast_if_needed ctx ct (c_expr ctx eelse) in
			IfVal(excond,ex1,ex2) ++ ct @@ e.epos
		| TBinop ( (Ast.OpAssign | Ast.OpAssignOp _ as op), e1, e2 ) ->
			let ex1 = clean_casts ctx (c_expr ctx e1) in
			let ct = c_type ctx e.etype in
			let ex2 = cast_if_needed ctx ct (c_expr ctx e2) in
			Binop(op, ex1, ex2) ++ ct @@ e.epos
		| TUnop ( (Increment | Decrement as op), flag, e1 ) ->
			let ex1 = clean_casts ctx (c_expr ctx e1) in
			let ct = c_type ctx e.etype in
			Unop(op, flag, ex1) ++ ct @@ e.epos
		(* | TField (ef, facc) -> *)
		(* | TUnop ( (Neg | NegBits as op), flag, e1) -> *)
		(* 	let ex1 = c_expr ctx e1 in *)
		(* 	let ct =  *)
		| _ -> assert false
	(* | Not -> "!" *)
	(* | Neg -> "-" *)
	(* | NegBits -> "~" *)
		(* | TCall of texpr * texpr list *)
		(* | TNew of tclass * tparams * texpr list *)
		(* | TFunction of tfunc *)
		(* | TVars of (tvar * texpr option) list *)
		(* | TBlock of texpr list *)
		(* | TFor of tvar * texpr * texpr *)
		(* | TWhile of texpr * texpr * Ast.while_flag *)
		(* | TSwitch of texpr * (texpr list * texpr) list * texpr option *)
		(* | TPatMatch of decision_tree *)
		(* | TTry of texpr * (tvar * texpr) list *)
		(* | TReturn of texpr option *)
		(* | TBreak *)
		(* | TContinue *)
		(* | TThrow of texpr *)

	(** default implementations **)
	(*****************************)

	(** enum conversion **)
	(* this (simple) version will: *)
		(* - transform simple enum fields into static readonly vars *)
		(* - transform parametered enum fields into static functions + ctor call *)
		(* - create parametered fields for the enum *)
		(* - add the metadata field if needed *)
		(* - ignore any type parameter, and consider them as Dynamic (OPTIMIZEME) *)

	let convert_simple_ef ctx e name ii =
		alloc_field
			~static:true
			~name
			~ftype:(mkt ~&(I32 true))
			~kind:(KVar (Some( I ii )) )
			~vis:VPublic
			~flags:( FPure |$ FEnum |$ FPublic )
			()

	let convert_simple_e ctx e =
		let i = ref 0 in
		let fields = List.map (fun name ->
			let ii = Int32.of_int !i in
			incr i;
			convert_simple_ef ctx e name ii
		) e.e_names in
		alloc_cls
			~path:e.e_path
			~fields
			(* ~super:ctx.econv.enum_super *)
			()

	(* let convert_ef ctx e ef = *)
	(* 	match follow ef.ef_type with *)
	(* 	| TFun(args,ret) -> *)

	(* let convert_complex_e ctx e = *)
	(* 	let i = ref 0 in *)
	(* 	let fields = List.map (fun name -> *)
	(* 		let ii = Int32.of_int !i in *)
	(* 		incr i; *)
	(* 		let ef = PMap.find name e.e_constrs in *)

	(* 	) e.e_name in *)


	let is_simple_enum e =
		not (List.exists (fun f ->
			let ctor = PMap.find f e.e_constrs in
			match follow ctor.ef_type with
				| TFun _ -> true
				| _ -> false
		) e.e_names)

	let default_e2c ctx =
		let enum_to_cls e =
			if is_simple_enum e then
				convert_simple_e ctx e
			else
				alloc_cls ~path:e.e_path ()
				(* convert_complex_e ctx e *)
		in
		enum_to_cls

	(* let default_econv ctx = *)
		(* { *)
			(* enum_to_cls = default_e2c ctx; *)
			(* enum_field_acc = default_efacc ctx; *)
		(* } *)

end;;

