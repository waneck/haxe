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
open GencommonType;;
open GencommonType.Filters;;
open GencommonType.Expr;;

(** Common filters and c-ast helpers **)

(** Mapping texpr to c-ast **)
(****************************)

module TExprMap =
struct
(* in order to map texpr into c-ast, we need to implement the following abstract structures: *)

	type type_to_ctype = {
		(* how types are mapped into ctypes *)
		tc_apply_params : module_type -> (string * t) list -> tparams -> t -> GencommonType.ct;
			(* some targets will map their type parameters differently *)
			(* (e.g. Java and its lack of support for basic types in tparams) *)

	}

	and extern_loading = {
		(* try to convert target-specific externs into more accurate types *)
		(* (using e.g. information from a native lib) *)
		try_load_extern : module_type -> cls option;
	}


(* context *)
	and ctx = {
		ccom : Common.context;
		load_type : module_type->cls;
	}
end;;
