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

(* typed AST used by Gencommon *)
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
	| Type of cls (* Class<> *)
	| Inst of cls * cparams
	| Value of valuetype

and pos = Ast.pos

and t = {
	ctype : ctype;
	cextra : unit option; (* extra flags about the type *)
}

and cparams = t array

and funprop =
	| GenericFunc
		(* is a generic function *)
	| VarFunc
	| PureFunc
	| VarArgs
	| Callconv of string

and intrinsic =
	(* general *)
	| IArrayDecl of t
	| IVectorDecl of t
	| INewVector
	| IVectorLen
	(* reflection *)
	| ICreateEmpty
	| ICreateEmptyConst of cls
	(* Std *)
	| IIs
	| IIsConst of cls
	| IAs of t (* warning: no value types possible *)
	| IRethrow
	| IGetFields

	(* others *)
	| ICustom of string

and expr_expr =
	| Const of const
	| Local of var
	| Cast of expr * t

	| FieldAcc of expr * tfield_access
	| StaticAcc of tfield_access
	| Call of expr * expr list
	| ArrayAcc of expr * expr

	| IfVal of expr * expr * expr
	| Binop of binop * expr * expr
	| Unop of unop * unop_flag * expr

	| Intrinsic of intrinsic * expr list
	| Function of func

and expr = {
	expr : expr_expr;
	t : t;
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

and stat_t =
	| VarDecl of var * expr option
	| If of expr * statement * statement option
	| While of expr * statement * Ast.while_flag
	| Switch of expr * (const list * statement) list * statement option
	| Try of statement * (var * statement) list
	| Return of expr option
	| Break of int
	| Continue of int
	| SwitchBreak of int
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
	a_parent : t;
	a_type : t;
	a_params : t array;
}

and tfield_access =
	| AClassField of field_access
	| ATypedExternal of t
	| ADynamic of t

and var = {
	vid : int;
	mutable vname : string;
	mutable vtype : t;
	mutable vwrite : bool;
		(* is it read-only? *)
	mutable vexpr : expr option;
	mutable vcaptured_by : func list;
}

and func = {
	fargs : (var * const option) list;
	fret : t;
	fexpr : statement;
	ftypes : tparam array;
}

and tparam = {
	mutable pname : string;
	pconstraints : t list;
}

and field = {
	fid : int;
	mutable fname : string;
	mutable fpublic : bool;
		(* declared public? *)
	mutable fvis : visibility Flags.t;
		(* actual (computed) visibility *)
	mutable ftype : t;
	mutable fstatic : bool;
	mutable foverrides : field option;
	mutable fkind : fkind;
	mutable fflags : fflag Flags.t;
	mutable fnative_modifiers : string list;
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
	| FExtern
		(* extern field: it doesn't really exist *)

and path = string list * string list * string
	(* package * nested types * name *)

and cls = {
	mutable tpath : path;
	mutable ttypes : tparam array;
	mutable tsuper : (cls * cparams) option;
	mutable tord_fields : field list;
	mutable tfields : (string, field) PMap.t;
	mutable timplements : (cls * cparams) list;

	(* "private" fields: DO NOT change them without calling the proper change function *)
	mutable tenclosing : cls option;
	mutable tnested : cls list;
}

type gen = {
	gcom : Common.context;

	mutable gfield : field;
		(* current class field running *)
	mutable gcur : cls;
		(* current class being mapped *)
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

	type typed_expr = expr_expr * t

	(* position operator. read as 'at pos' *)
	(* constructs expressions from typed_expr and the position *)
	let (@@) (e,t) p = mk e t p

	(* pair creation operator *)
	let (++) e t = (e, t)

	(* typed expression operator. read as 'typed with type' *)
	let (+:) e t = (e, mkt t)

	(* value type unary *)
	let (!%) t = Value t

	let sample_expr p : expr =
		Binop(OpAdd, Const(S"Hello, ") +: String @@ p, Const(S"World!") +: String @@ p) +: String @@ p

	let mkcls_params cls =
		Array.init (Array.length cls.ttypes) (fun i -> mkt !%(TypeParam i))

	let mkthis gen = match gen.gfield.fstatic with
	| true ->
		Const(Class gen.gcur) +: Type gen.gcur
	| false ->
		Const This +: Inst(gen.gcur, mkcls_params gen.gcur)

end;;
