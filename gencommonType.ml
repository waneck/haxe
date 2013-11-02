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

(* typed AST used by Gencommon *)
(* its main purpose is to be much closer to what the statically typed target can understand *)
type valuetype =
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
	| String
	| Dynamic
	| Pointer of ctype
	| Vector of ctype
	| Array of ctype
	| Null of valuetype
	| Fun of callconv list * ctype * (ctype list)
	| Type of ctype (* Class<> *)
	| Inst of cls * cparams
	| Value of valuetype

and pos = Ast.pos

and t = {
	ctype : ctype;
	cextra : unit option; (* extra flags about the type *)
}

and cparams = t list

and callconv =
	| GenericFunc
	| None

and expr =
	| Const of tconstant * t
	| Local of var
	| Cast of expr * t

	| FieldAcc of expr * tfield_access
	| StaticAcc of tfield_access
	| Call of expr * expr_pos list
	| ArrayAcc of expr * expr

	| IfVal of expr * expr_pos * expr_pos
	| Binop of Ast.binop * expr * expr * pos
	| Unop of Ast.unop * Ast.unop_flag * expr

	| ArrayDecl of t * expr_pos list
	| VarDecl of var * expr option

	| Function of func

and expr_pos = expr * pos

and stat_t =
	| If of expr * statement * statement option
	| While of expr * statement * Ast.while_flag
	| Switch of expr * (tconstant list * statement) list * statement option
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
	fargs : (var * tconstant option) list;
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
	mutable ftype : t;
	mutable fstatic : bool;
	mutable foverrides : field option;
	mutable fkind : fkind;
	(* flags *)
	mutable fconstant : bool; (* strict: if on, optimizations can be performed *)
	mutable fextern : bool;
}

and fkind =
	| KVar
	| KVarProp of bool * bool (* read, write *)
	| KProp of bool * bool (* read, write *)
	| KMethod of func

and path = string list * string list * string
	(* package * nested types * name *)

and cls = {
	mutable tpath : path;
	mutable tsuper : (cls * cparams) option;
	mutable tord_fields : field list;
	mutable tfields : (string, field) PMap.t;
	mutable timplements : (cls * cparams) list;

	(* "private" fields: DO NOT change them without calling the proper change function *)
	mutable tenclosing : cls option;
	mutable tnested : cls list;
}
