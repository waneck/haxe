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
open GencommonType;;
open GencommonType.Filters;;
open GencommonType.Expr;;
open GencommonType.Helpers;;

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

		(* internal props *)
		mutable cur_class : tclass;
		mutable cur_field : tclass_field option;
		mutable class_types : type_params;
		mutable fun_stack : (func * type_params) list;
			(* stack of functions *)
		mutable var_map : (int, var) PMap.t;
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
		match ctx.tconv.map_params with
		| None ->
			Array.of_list (List.map (c_type ctx) tparams)
		| Some m ->
			m site tparams

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
				c_type ctx (apply_params t.t_types tl t.t_type))
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
			mktr (Fun( [VarFunc], c_type ctx ret, List.map (fun (n,o,t) ->
				c_type ctx t
			) args ))
		| TAbstract( ({ a_impl = Some _ } as a),p) ->
			c_type ctx (Codegen.Abstract.get_underlying_type a p)
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

	let c_const ctx = function
		| TInt i -> I i
		| TFloat f -> F f
		| TString s -> S s
		| TBool b -> B b
		| TNull -> Nil
		| TThis -> This
		| TSuper -> Super

	let c_var ctx v = try
		PMap.find v.v_id ctx.var_map
	with | Not_found ->
		let vconv = GencommonType.alloc_var
			~name:v.v_name
			~vtype:(c_type ctx v.v_type)
			~kind:VUndeclared (* Not_found here always means it's undeclared *)
			()
		in
		ctx.var_map <- PMap.add v.v_id vconv ctx.var_map;
		vconv

	let is_intrinsic name =
		String.length name > 2 &&
		String.get name 0 = '_' &&
		String.get name 1 = '_'

	let rec c_expr ctx ?stype e = match e.eexpr with
		| TConst c -> Const( c_const ctx c ) ++ (c_type ctx e.etype) @@ e.epos
		| TLocal v -> (try
			let vconv = try
					PMap.find v.v_id ctx.var_map
				with | Not_found when not (is_intrinsic v.v_name) -> (* check if intrinsic *)
					let vconv = GencommonType.alloc_var
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
			Const( Class ct ) *: Type ct @@ e.epos
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
		(* | TArray (e1,e2) -> *)
		(* 	let ex1 = c_expr ctx e1 in *)
		(* 	let ex2 = c_expr ctx e2 in *)
		(* 	let t = match ex1.t.ctype with *)
		(* 		| Pointer ct | Array ct | Vector ct -> ct *)
		(* 		| Inst(c,p) | V(Struct(c,p)) *)
		(* 		| Null(Struct(c,p)) -> (try *)
		(* 			let f = PMap.find "__array" c.cvars in *)


		(* | TIf of texpr * texpr * texpr option *)
		(* | TCast of texpr * module_type option *)
		(* | TMeta of metadata_entry * texpr *)
		(* | TEnumParameter of texpr * tenum_field * int *)
		| _ -> assert false
		(* | TBinop of Ast.binop * texpr * texpr *)
		(* | TField of texpr * tfield_access *)
		(* | TCall of texpr * texpr list *)
		(* | TNew of tclass * tparams * texpr list *)
		(* | TUnop of Ast.unop * Ast.unop_flag * texpr *)
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
			~public:true
			~vis:VPublic
			~flags:( FPure |$ FEnum )
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

