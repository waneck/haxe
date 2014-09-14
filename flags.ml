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

type ('a) t = int

let empty : 'a t = 0

let tag (v : 'a) : int = 1 lsl (Obj.magic v)

let is_empty (this : 'a t) = this = 0

let set (this : 'a t) v =
	this lor (tag v)

let unset (this : 'a t) v =
	this land (lnot (tag v))

let has (this : 'a t) v =
	let t = tag v in
	this land t = t

let has_all (this : 'a t) vl =
	List.for_all (has this) vl

let has_any (this : 'a t) vl =
	List.exists (has this) vl

let from_list vl =
	List.fold_left (fun ret v -> set ret v) 0 vl

let (|+) (this : 'a t) v = set this v

let (|$) v1 v2 = set empty v1 |+ v2
