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
		enum_to_cls : tenum -> cls;
		enum_field_acc : texpr -> tfield_access;
			(* texpr will be either a TField or a TCall( TField, _ ) *)
	}

	and anon_conversion = {
		anon_to_ct : t -> ct;
			(* this conversion will make an effort to preserve typedefs *)
		anon_field_acc : texpr -> tfield_access;
			(* texpr will be either a TField or a TCall( TField, _ ) *)
	}

	and site_info =
		| ClassSite of module_type
		| FieldSite of module_type * tclass_field
		| EnumFieldSite of tenum * tenum_field
		| AnonFunctionSite of tfunc * type_params


(* context *)
	and ctx = {
		ccom : Common.context;
		load_type : module_type->cls;

		mutable extern : extern_loading option;
		mutable econv : enum_conversion;
		mutable aconv : anon_conversion;
		mutable tconv : type_conversion;
	}


	(** default implementations **)
	(*****************************)

	(** enum conversion **)
	let convert_simple_e ctx e =
		let c = mk_cls ~path:e.e_path () in
		c

	let default_e2c ctx =
		let enum_to_cls e =
			()
		in
		()

	let default_econv ctx =
		{
			enum_to_cls = default_e2c ctx;
			enum_field_acc = default_efacc ctx;
		}

end;;

