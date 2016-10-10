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

(**
 * Haxe object file implementation - shared definitions
 **)

(* current file version. versions need to match exactly *)
let version = 1

(* constant table entry *)
type const_table =
	| TblString of string
	| TblPath of path
	| TblModuleRef of path * string (* module.typeName *)
	| TblType of t
	| TblVar of tvar



(* and type_params = (string * t) list *)
(*  *)
(* and tconstant = *)
(* 	| TInt of int32 *)
(* 	| TFloat of string *)
(* 	| TString of string *)
(* 	| TBool of bool *)
(* 	| TNull *)
(* 	| TThis *)
(* 	| TSuper *)
(*  *)
(* and tvar_extra = (type_params * texpr option) option *)
(*  *)
(* and tvar = { *)
(* 	mutable v_id : int; *)
(* 	mutable v_name : string; *)
(* 	mutable v_type : t; *)
(* 	mutable v_capture : bool; *)
(* 	mutable v_extra : tvar_extra; *)
(* 	mutable v_meta : metadata; *)
(* 	v_pos : pos; *)
(* } *)
(*  *)
