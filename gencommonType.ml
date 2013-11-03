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
	| Int8 of bool (* signed : bool *)
	| Int16 of bool (* signed : bool *)
	| Int32 of bool (* signed : bool *)
	| Int64 of bool (* signed : bool *)
	| IntPtr of bool (* signed : bool *)
	| Float32
	| Float64
	| TypeParam of int
	| MethodTypeParam of int
	| Struct of cls * cparams

and ctype =
	(* any type that has a simplified form should not be referred using the longer cls * cparams form *)
	| String
	| Dynamic
	| Pointer of ctype
	| Vector of ctype
	| Array of ctype
	| Null of valuetype
	| Fun of funprop list * ctype * (ctype list)
	| Type of cls option (* Class<>; Type None means it's Class<Dynamic> *)
	| Inst of cls * cparams
	| Value of valuetype

and pos = Ast.pos

and ct = {
	ctype : ctype;
	cextra : unit option; (* extra flags about the type *)
}

and cparams = ct array

and funprop =
	| GenericFunc
		(* is a generic function *)
	| VarFunc
	| PureFunc
	| VarArgs
	| Callconv of string

and intrinsic =
	(* general *)
	| IArrayDecl of ct
	| IVectorDecl of ct
	| INewVector of ct
	| IVectorLen
	(* reflection *)
	| ICreateEmpty
	| ICreateEmptyConst of cls
	(* Std *)
	| IIs
	| IIsConst of cls
	| IAs of ct (* warning: no value types possible *)
	| IRethrow
	| IGetFields

	(* others *)
	| ICustom of string

and expr_expr =
	| Const of const
	| Local of var
	| Cast of expr * ct

	| FieldAcc of expr * tfield_access
	| StaticAcc of tfield_access
	| Call of expr * expr list
	| ArrayAcc of expr * expr

	| IfVal of expr * expr * expr
	| Binop of binop * expr * expr
	| Unop of unop * unop_flag * expr

	| Intrinsic of intrinsic * expr list
	| Function of func

and stat_t =
	| VarDecl of var * expr option
	| If of expr * statement * statement option
	| While of expr * statement * Ast.while_flag
	| Switch of expr * (const list * statement * bool) list * statement option
		(* switch (cond) * ( constants: statment * fallthrough:bool) * statement *)
	| Try of statement * (var * statement) list
	| Return of expr option
	| Break of int
	| Continue of int
	| Throw of expr
	| SExpr of expr

and statement = {
	s_id : int;
	mutable s_temps : var list;
	mutable s_declared_here : var list;
	mutable s_block : stat_t list;
}

and field_access = {
	a_field : field;
	a_parent : ct;
	a_type : ct;
	a_params : ct array;
}

and expr = {
	expr : expr_expr;
	t : ct;
	pos : pos;
}

and const =
	| S of string (* String *)
	| I of int32 (* Int *)
	| D of string (* Float *)
	| F of string (* Single *)
	| B of bool
	| Nil (* null *)
	| This
	| Class of cls
	| Super

and tfield_access =
	| AClassField of field_access
	(* | ATypedExternal of ct *)
	| ADynamic of ct

and var = {
	vid : int;
	mutable vname : string;
	mutable vtype : ct;
	mutable vwrite : bool;
		(* is it read-only? *)
	mutable vexpr : expr option;
	mutable vcaptured_by : func list;
}

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
}

and fkind =
	| KVar
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
	| FWriteAccess
		(* was accessed for writing *)
	| FReadAccess
		(* was accessed for reading *)

and path = string list * string
	(* package * nested types * name *)

and cls = {
	cid : int;
	mutable tpath : path;
	mutable ttypes : tparam array;
	mutable tsuper : (cls * cparams) option;
	mutable tord_fields : field list;
	mutable tord_methods : field list;
	mutable tfields : (string, field) PMap.t;
	mutable tmethods : (string, field list) PMap.t;
	mutable timplements : (cls * cparams) list;

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
}

(* all expression helpers go here *)
module Expr =
struct

	let mk e t p = { expr = e; t = t; pos = p; }

	let mkt t = {
		ctype = t;
		cextra = None;
	}

	let mkt_vt vt = {
		ctype = Value vt;
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

	(* value type unary *)
	let (!&) t = Value t

	let sample_expr p : expr =
		Binop(OpAdd, Const(S"Hello, ") +: String @@ p, Const(S"World!") +: String @@ p) +: String @@ p

	let mkcls_params cls =
		Array.init (Array.length cls.ttypes) (fun i -> mkt !&(TypeParam i))

	let mkthis gen = match gen.gfield.fstatic with
	| true ->
		Const(Class gen.gcur) +: Type (Some gen.gcur)
	| false ->
		Const This +: Inst(gen.gcur, mkcls_params gen.gcur)

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
let mk_filter_ctx
		?expr_map
		?enter_stmt
		?exit_stmt
		?shallow_stmt_map () =
	{
		expr_map = expr_map;
		enter_stmt = enter_stmt;
		exit_stmt = exit_stmt;
		shallow_stmt_map = shallow_stmt_map;
	}

let cls_id = ref 0

let mk_cls
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
	incr cls_id;
	let id = !cls_id in
	let fields = List.fold_left (fun map f ->
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
	{
		cid = id;
		tpath = path;
		ttypes = types;
		tsuper = super;
		tord_fields = ord_fields;
		tord_methods = ord_methods;
		tfields = fields;
		tmethods = methods;
		timplements = implements;

		tenclosing = None;
		tnested = [];
	}

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

let mk_field
		~static
		~name
		~ftype
		~kind
		?(public=false)
		?(vis=VPrivate)
		?override
		?(flags = Flags.empty)
		?(modifiers = []) () =

	incr field_id;
	let id = !field_id in
	{
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
	}


