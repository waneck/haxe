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

(* simple bit flag that maps parameterless flags to an int *)
(* warning: only use this if you know what you're doing *)

type ('a) t

val empty : ('a) t

val is_empty : ('a) t -> bool

val set : ('a) t -> 'a -> ('a) t

val unset : ('a) t -> 'a -> ('a) t

val has : ('a) t -> 'a -> bool

val has_all : ('a) t -> ('a) list -> bool

val has_any : ('a) t -> ('a) list -> bool

val from_list : ('a) list -> ('a) t

val (|+) : ('a) t -> 'a -> ('a) t

val (|$) : 'a -> 'a -> ('a) t
