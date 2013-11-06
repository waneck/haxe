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
open Type;;
open Flags;;
open Ast;;

(* typed AST used by Gencommon : called c-ast herein *)
(* its main purpose is to be much closer to what the statically typed target can understand *)
(* it also is designed to: *)
	(* generate less garbage on filters *)
	(* allow optimizations to happen *)
	(* make it easier (and cleaner) to generate new expressions *)

type valuetype =
	(* any type that has a simplified form should not be referred using the longer cls * cparams form *)
	| Void
	| Bool
	| Char
	| I8 of bool (* signed : bool *)
	| I16 of bool (* signed : bool *)
	| I32 of bool (* signed : bool *)
	| I64 of bool (* signed : bool *)
	| IntPtr of bool (* signed : bool *)
	| F32
	| F64
	| TypeParam of int (* type param number *)
	| MethodTypeParam of int * int (* function stack * type param number *)
	| Struct of cls * cparams

and reftype =
	(* any type that has a simplified form should not be referred using the longer cls * cparams form *)
	| String
	| Dynamic
	| Pointer of ct
	| Vector of ct
	| Array of ct
	| Type of ct
	| Inst of cls * cparams
	| Fun of funprop list * ct * (ct list)

and ctype =
	| R of reftype
		(* reference type *)
	| V of valuetype
		(* value type *)
	| Null of valuetype
		(* Null<> type *)
	| SpecializedNull of ct
		(* this type should be "ephemeral": it should only appear in type parameters *)
		(* or as a return-type or paremeter of a specialized function. *)
		(* It indicates that the type was a result of a specialized Null<TypeParameter> type *)
		(* under any other condition, the type should be replaced by the actual `ctype`, as Null<ReferenceType> *)
		(* should be the same as ReferenceType *)

and pos = Ast.pos

and ct = {
	ctype : ctype;
	cextra : extra_type_info option; (* extra flags about the type *)
}

and extra_type_info = {
	(* put here everything that we'd instead use abstracts or typedefs to annotate *)
	et_reserved : unit;
}

and cparams = ct array

and funprop =
	| GenericFunc
		(* is a generic function *)
	| VarFunc
	| PureFunc
	| SideEffectsFree
	| VarArgs
	| Callconv of string

and intrinsic =
	(* general *)
	| IArrayDecl of ct
	| IVectorDecl of ct
	| INewVector of ct
	| IVectorLen
	| IUndefined
	(* reflection *)
	| ICreateEmpty
	| ICreateEmptyConst of ct
	(* Std *)
	| IIs
	| IIsConst of ct
	| IAs of ct (* warning: no value types possible *)
	| IRethrow
	| IGetFields

	(* others *)
	| ICustom of string

and expr_expr =
	| Const of const
	| Local of var
	| Cast of expr * cast_safety

	| FieldAcc of expr * tfield_access
	| StaticAcc of tfield_access
	| Call of expr * expr list
	| ArrayAcc of expr * expr * array_access

	| IfVal of expr * expr * expr
	| Binop of binop * expr * expr
	| Unop of unop * unop_flag * expr

	| Intrinsic of intrinsic * expr list
	| Function of func

and stat_t =
	| VarDecl of var * expr option
	| If of expr * statement * statement option
	| While of expr * statement * Ast.while_flag
	| Switch of expr * (const list * statement * switch_case_flag) list * statement option
	| Try of statement * (var * statement) list
	| Return of expr option
	| Break
	| Continue
	| Throw of expr
	| SExpr of expr

and statement = {
	s_id : int;
	mutable s_old_id : int option;
		(* if this statement was mapped, s_old_id will hold the parent id *)
	mutable s_temps : var list;
	mutable s_declared_here : var list;
	mutable s_block : stat_t list;
}

and field_access = {
	a_field : field;
		(* accessed field *)
	(* a_expected : ct; *)
		(* TODO: the expected type as defined by the original texpr *)
	(* a_actual : ct; *)
		(* TODO: the actual type, with the applied params *)
	a_params : ct array;
		(* applied function type parameters for this function access *)
}

and cast_safety =
	| SafeCast
		(* cast(a, SomeType) -> needs runtime check *)
	| UnsafeCast
		(* var b:SomeType = cast a; -> doesn't need runtime check *)

and switch_case_flag =
	| NormalCase
	| Fallthrough

and expr = {
	expr : expr_expr;
	t : ct;
	pos : pos;
}

and const =
	| S of string (* String *)
	| I of int32 (* Int *)
	| F of string (* Float *)
	| C of char (* Char *)
	| B of bool
	| Nil (* null *)
	| This
	| Class of ct
	| Super

and tfield_access =
	| AClassField of field_access
	(* | ATypedExternal of ct *)
	| ADynamic of ct * bool (* bool determines if it should throw exceptions on type not found *)

and array_access =
	| ArrBuiltin
	| ArrClassField of field_access
		(* by convention, we will look for the __array property. *)
	| ArrNotFound
	| ArrDynamic of ct

and var = {
	vid : int;
	mutable vname : string;
	mutable vtype : ct;
	mutable vwrite : bool;
		(* is it read-only? *)
	mutable vexpr : expr option;
	mutable vcaptured_by : func list;
	mutable vmax : expr option;
		(* max value constrained *)
	mutable vmin : expr option;
		(* min value constrained *)
	mutable vkind : var_kind;
}

and var_kind =
	| VUndeclared
	| VNormal
	| VLoopVar
	| VFunArg

and func = {
	fargs : (var * fun_arg) list;
	fret : ct;
	fexpr : statement;
	ftypes : tparam array;
}

and fun_arg = {
	aopt : const option;
	akind : arg_kind;
}

and arg_kind =
	| ANormal
	| AOut
	| ARef

and tparam = {
	mutable pname : string;
	pconstraints : ct list;
}

and field = {
	fid : int;
	mutable fname : string;
	mutable fpublic : bool;
		(* declared public? *)
	mutable fvis : visibility;
		(* actual (computed) visibility *)
	mutable ftype : ct;
	mutable fstatic : bool;
	mutable foverride : field option;
	mutable fkind : fkind;
	mutable fflags : fflag Flags.t;
	mutable fmodifiers : string list;
	mutable fdeclared : cls;
}

and fkind =
	| KVar of const option
		(* a normal var *)
	| KVarProp of bool * bool (* read, write *)
		(* a property (getter/setter call) and also an underlying var *)
	| KProp of bool * bool (* read, write *)
		(* only a property *)
	| KMethod of func

and visibility =
	| VPublic
	| VInternal
		(* only visible in this assembly *)
	| VNested
		(* only visible to enclosing type *)
	| VProtected
	| VPrivate

and fflag =
	| FPure (* strict: if on, optimizations can be performed *)
		(* it means that it's a pure read-only value, so the order in which it's called *)
		(* doesn't matter, and results can be cached *)
	| FSideEffectsFree
		(* the call doesn't affect any state *)
	| FWriteAccess
		(* was accessed for writing *)
	| FReadAccess
		(* was accessed for reading *)
	| FEnum
		(* was an enum field *)

and path = string list * string
	(* package * nested types * name *)

and cls = {
	cid : int;
	mutable cpath : path;
	mutable ctypes : tparam array;
	mutable csuper : (cls * cparams) option;
	mutable cord_fields : field list;
	mutable cvars : (string, field) PMap.t;
	mutable cmethods : (string, field list) PMap.t;
	mutable cimplements : (cls * cparams) list;

	(* "private" fields: DO NOT change them without calling the proper change function *)
	mutable tenclosing : cls option;
	mutable tnested : cls list;
}

(** Generator **)

type gen = {
	gcom : Common.context;

	mutable gfield : field;
		(* current class field running *)
	mutable gcur : cls;
		(* current class being mapped *)

	mutable filters : (string * float * filter) list;
	mutable filters_dirty : bool;
}

and filter = gen->filter_ctx
(* the mapper calls the "gen" function for each new field being mapped *)

and filter_ctx = {
	(* a filter context determines how and if the mapper will map the expressions *)
	(* and statements. setting any of them as None means that the filter provides *)
	(* no implementation for it *)
	expr_map : (expr->expr) option;
		(* maps an expression into another expression *)
	enter_stmt : (statement->unit) option;
		(* is called when the mapper enters a statement *)
	exit_stmt : (statement->unit) option;
		(* is called when the mapper exits a statement *)
	shallow_stmt_map : (statement->statement) option;
		(* maps a statement into another statement *)
		(* the implementation assumes that it's a shallow map - *)
		(* meaning that the inner expressions will still be mapped *)
	stmt_map : (stat_t->stat_t) option;
}

(* all expression helpers go here *)
module Expr =
struct

	let mk e t p = { expr = e; t = t; pos = p; }

	let mkt t = {
		ctype = t;
		cextra = None;
	}

	let mktr r = {
		ctype = R r;
		cextra = None;
	}

	let mktv vt = {
		ctype = V vt;
		cextra = None;
	}

	type typed_expr = expr_expr * ct

	(* position operator. read as 'at pos' *)
	(* constructs expressions from typed_expr and the position *)
	let (@@) (e,t) p = mk e t p

	(* pair creation operator *)
	let (++) e t = (e, t)

	(* typed expression operator. read as 'typed with type' *)
	let (+:) e t = (e, mkt t)
	let (+*) e t = (e, mktr t)
	let (+&) e t = (e, mktv t)

	(* value type unary *)
	let (~&) t = V t

	(* ref type unary *)
	let (~*) t = R t

	let sample_expr p : expr =
		Binop(OpAdd, Const(S"Hello, ") +: ~*String @@ p, Const(S"World!") +: ~*String @@ p) +: ~*String @@ p

	let mkcls_params cls =
		Array.init (Array.length cls.ctypes) (fun i -> mkt ~&(TypeParam i))

	let mkthis gen = match gen.gfield.fstatic with
	| true ->
		let inst = mktr (Inst(gen.gcur, mkcls_params gen.gcur)) in
		Const(Class inst) +* Type inst
	| false ->
		Const This +* Inst(gen.gcur, mkcls_params gen.gcur)

	let combine_cextra cextra_orig cextra = match cextra_orig, cextra with
		| None, None -> None
		| None, Some{ et_reserved = _ } -> None
		| Some{ et_reserved = _ }, None -> None
		| Some{ et_reserved = _ }, Some { et_reserved = _ } -> None

	let combine_ct_cextra ct cextra_orig =
		let combined = combine_cextra cextra_orig ct.cextra in
		if combined = ct.cextra then
			ct
		else
			{ ct with cextra = combined }

	let rec ensure_toplevel ct = match ct.ctype with
		| SpecializedNull n -> combine_ct_cextra (ensure_toplevel n) ct.cextra
		| t -> ct

	let rec apply_cparams ?(toplevel=true) cls params ct = match ct.ctype with
		| V( Struct (c,p) ) ->
			{ ct with ctype = V( Struct(c, Array.map (apply_cparams ~toplevel:false cls params) p) ) }
		| V( TypeParam i ) ->
			let ret = params.(i) in
			combine_ct_cextra ret ct.cextra
		| Null( TypeParam i ) -> (match toplevel, params.(i) with
			| true, { ctype = V v; cextra = e } ->
				{ ctype = Null( v ); cextra = combine_cextra ct.cextra e }
			| false, r ->
				{ ctype = SpecializedNull( r ); cextra = combine_cextra ct.cextra r.cextra }
			| true, v ->
				ensure_toplevel v)
		| R(Fun(pl,ret,args)) ->
			{ ct with ctype = ~*(Fun(pl,apply_cparams ~toplevel:false cls params ret, List.map (apply_cparams ~toplevel:false cls params) args)) }
		| R(Pointer p) ->
			{ ct with ctype = ~*(Pointer(apply_cparams ~toplevel:false cls params p)) }
		| R(Vector v) ->
			{ ct with ctype = ~*(Vector(apply_cparams ~toplevel:false cls params v)) }
		| R(Array v) ->
			{ ct with ctype = ~*(Array(apply_cparams ~toplevel:false cls params v)) }
		| R(Type v) ->
			{ ct with ctype = ~*(Type(apply_cparams ~toplevel:false cls params v)) }
		| R(Inst(c,p)) ->
			{ ct with ctype = ~*(Inst(c, Array.map (apply_cparams ~toplevel:false cls params) p)) }
		| SpecializedNull(t) ->
			if toplevel then
				ensure_toplevel t
			else
				let ret = apply_cparams ~toplevel:false cls params t in
				{ ct with ctype = SpecializedNull(ret) }
		| v -> ct


end;;

module Helpers =
(* some helpers and extension methods *)
struct

	let rev_filter_map fn lst =
		let rec loop acc = function
			| [] -> acc
			| v :: lst -> match fn v with
			| None -> loop acc lst
			| Some v -> loop (v :: acc) lst
		in
		loop [] lst

	let filter_map fn lst =
		List.rev (rev_filter_map fn lst)

	let array_empty () =
		Array.init 0 (fun _ -> assert false)

	let opt_or_empty = function
		| Some v -> v
		| None -> array_empty()

	let path_s path =
		match path with | ([], s) -> s | (p, s) -> (String.concat "." (fst path)) ^ "." ^ (snd path)

	let listfind_i fn lst =
		let rec loop i lst = match lst with
			| [] -> raise Not_found
			| hd :: tl -> if fn hd then
				i
			else
				loop (i+1) tl
		in
		loop 0 lst
end;;
open Helpers;;

type t_dependency =
	| DAfter of float
	| DBefore of float

module Filters =
struct

	exception ImpossibleDependency of string

	let max_dep = 10000.0
	let min_dep = - (10000.0)

	let solve_deps name (deps:t_dependency list) =
		let vmin = min_dep -. 1.0 in
		let vmax = max_dep +. 1.0 in
		let rec loop dep vmin vmax =
			match dep with
			| [] ->
				if vmin >= vmax then raise (ImpossibleDependency name);
				(vmin +. vmax) /. 2.0
			| head :: tail ->
				match head with
				| DBefore f ->
					loop tail (max vmin f) vmax
				| DAfter f ->
					loop tail vmin (min vmax f)
		in
		loop deps vmin vmax

	let add_filter gen name priority filter =
		gen.filters <- (name,priority,filter) :: gen.filters;
		gen.filters_dirty <- true

	let run_filters gen field =
		(* sort by priority *)
		if gen.filters_dirty then begin
			gen.filters <- List.sort (fun (_,f1,_) (_,f2,_) -> compare f1 f2) gen.filters;
			gen.filters_dirty <- false
		end;
		(* TODO: not implemented yet *)
		()

end;;

(* boilerplate *)
let get_id idref =
	let i = !idref in
	incr idref;
	i

let alloc_filter_ctx
		?expr_map
		?enter_stmt
		?exit_stmt
		?shallow_stmt_map
		?stmt_map () =
	{
		expr_map = expr_map;
		enter_stmt = enter_stmt;
		exit_stmt = exit_stmt;
		shallow_stmt_map = shallow_stmt_map;
		stmt_map = stmt_map;
	}

let cls_id = ref 0

let alloc_cls
		~path
		?types
		?super
		?(fields=[])
		?(implements=[]) () =

	let types = opt_or_empty types in
	let ord_fields, ord_methods =
		List.partition (function
			| { fkind = KMethod _ } -> false
			| _ -> true
		) fields
	in
	let id = get_id cls_id in
	let mapfields = List.fold_left (fun map f ->
		assert (not (PMap.mem f.fname map));
		PMap.add f.fname f map
	) PMap.empty ord_fields
	in
	let methods = List.fold_left (fun map f ->
		let lst = try
			PMap.find f.fname map
		with | Not_found ->
			[]
		in
		PMap.add f.fname (f :: lst) map
	) PMap.empty ord_methods in
	let ret = {
		cid = id;
		cpath = path;
		ctypes = types;
		csuper = super;
		cord_fields = fields;
		cvars = mapfields;
		cmethods = methods;
		cimplements = implements;

		tenclosing = None;
		tnested = [];
	} in
	List.iter (fun f ->
		f.fdeclared <- ret
	) fields;
	ret

let null_path = ([],"<null>")

let null_cls = alloc_cls ~path:(null_path) ()

let add_field cls field =
	assert (field.fdeclared == null_cls);
	field.fdeclared <- cls;
	cls.cord_fields <- field :: cls.cord_fields;
	match field.fkind with
	| KMethod _ ->
		let v = try
			PMap.find field.fname cls.cmethods
		with | Not_found ->
			[]
		in
		cls.cmethods <- PMap.add field.fname (field :: v) cls.cmethods
	| _ ->
		cls.cvars <- PMap.add field.fname field cls.cvars

let field_detach field =
	let cls = field.fdeclared in
	field.fdeclared <- null_cls;
	cls.cord_fields <- List.filter (fun f -> f == field) cls.cord_fields;
	match field.fkind with
	| KMethod _ ->
		let v = try
			PMap.find field.fname cls.cmethods
		with | Not_found ->
			assert false
		in
		let v, rest = List.partition (fun f -> f == field) v in
		assert (v <> []);
		cls.cmethods <- PMap.add field.fname rest cls.cmethods
	| _ ->
		cls.cvars <- PMap.remove field.fname cls.cvars

let add_child ~parent ~child =
	assert (child.tenclosing = None);
	child.tenclosing <- Some parent;
	parent.tnested <- child :: parent.tnested

let remove_child ~parent ~child =
	assert (child.tenclosing = Some parent);
	child.tenclosing <- None;
	let c, others = List.partition (fun v -> v == child) parent.tnested in
	assert (c <> []);
	parent.tnested <- others

let field_id = ref 0

let alloc_field
		~static
		~name
		~ftype
		~kind
		?(public=false)
		?(vis=VPrivate)
		?override
		?(flags = Flags.empty)
		?(modifiers = [])
		?(declared = null_cls) () =

	let id = get_id field_id in
	let f = {
		fid = id;
		fname = name;
		fpublic = public;
		fvis = vis;
		ftype = ftype;
		fstatic = static;
		foverride = override;
		fkind = kind;
		fflags = flags;
		fmodifiers = modifiers;
		fdeclared = null_cls;
	} in
	if declared != null_cls then
		add_field declared f;
	f

let stmt_id = ref 0

let alloc_stmt
		?old_id
		?(block=[])
		() =

	let id = get_id stmt_id in
	{
		s_id = id;
		s_old_id = old_id;
		s_temps = [];
		s_declared_here = [];
		s_block = block;
	}

let var_id = ref 0

let alloc_var
		~name
		~vtype
		~kind
		?expr
		() =

	let id = get_id var_id in
	{
		vid = id;
		vname = name;
		vtype = vtype;
		vexpr = expr;
		vwrite = false;
		vcaptured_by = [];
		vmax = None;
		vmin = None;
		vkind = kind;
	}

let get_intrinsic name ct args = match name, ct.ctype, args with
	| "__array__", R(Array ct), _ -> IArrayDecl ct
	| "__vec__", R(Vector ct), _ -> IVectorDecl ct
	| "__newvec__", R(Vector ct), _ -> INewVector ct
	| "__veclen__", _, _ -> IVectorLen
	| "__undefined__", _, _ -> IUndefined
	| "__empty__", R Dynamic, _ -> ICreateEmpty
	| "__empty__", _, _ -> ICreateEmptyConst ct
	| "__is__", _, [_, { expr = Const (Class c) }] ->
			IIsConst c
	| "__is__", _, _ -> IIs
	| "__as__", _, [_, { expr = Const (Class c) }] ->
			IAs c
	| "__rethrow__", _, _ -> IRethrow
	| "__fields__", _, _ -> IGetFields
	| _ -> ICustom name

(* FIXME: implement this *)
let cast_if_needed ctx to_ct expr = expr
