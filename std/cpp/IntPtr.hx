/*
 * Copyright (C)2005-2016 Haxe Foundation
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
 */
package cpp;

@:coreType @:notNull @:runtimeValue abstract IntPtr from Int {
	@:op(A+B) public static function addI(lhs:IntPtr, rhs:Int):IntPtr;
	@:op(A+B) public static function add(lhs:IntPtr, rhs:IntPtr):IntPtr;
	@:op(A*B) public static function mulI(lhs:IntPtr, rhs:Int):IntPtr;
	@:op(A*B) public static function mul(lhs:IntPtr, rhs:IntPtr):IntPtr;
	@:op(A%B) public static function modI(lhs:IntPtr, rhs:Int):IntPtr;
	@:op(A%B) public static function mod(lhs:IntPtr, rhs:IntPtr):IntPtr;
	@:op(A-B) public static function subI(lhs:IntPtr, rhs:Int):IntPtr;
	@:op(A-B) public static function sub(lhs:IntPtr, rhs:IntPtr):IntPtr;
	@:op(A/B) public static function divI(lhs:IntPtr, rhs:Int):IntPtr;
	@:op(A/B) public static function div(lhs:IntPtr, rhs:IntPtr):IntPtr;
	@:op(A|B) public static function orI(lhs:IntPtr, rhs:Int):IntPtr;
	@:op(A|B) public static function or(lhs:IntPtr, rhs:IntPtr):IntPtr;
	@:op(A^B) public static function xorI(lhs:IntPtr, rhs:Int):IntPtr;
	@:op(A^B) public static function xor(lhs:IntPtr, rhs:IntPtr):IntPtr;
	@:op(A&B) public static function andI(lhs:IntPtr, rhs:Int):IntPtr;
	@:op(A&B) public static function and(lhs:IntPtr, rhs:IntPtr):IntPtr;
	@:op(A<<B) public static function shlI(lhs:IntPtr, rhs:Int):IntPtr;
	@:op(A<<B) public static function shl(lhs:IntPtr, rhs:IntPtr):IntPtr;
	@:op(A>>B) public static function shrI(lhs:IntPtr, rhs:Int):IntPtr;
	@:op(A>>B) public static function shr(lhs:IntPtr, rhs:IntPtr):IntPtr;

	@:op(A>B) public static function gt(lhs:IntPtr, rhs:IntPtr):Bool;
	@:op(A>=B) public static function gte(lhs:IntPtr, rhs:IntPtr):Bool;
	@:op(A<B) public static function lt(lhs:IntPtr, rhs:IntPtr):Bool;
	@:op(A<=B) public static function lte(lhs:IntPtr, rhs:IntPtr):Bool;

	@:op(~A) public static function bneg(t:IntPtr):IntPtr;
	@:op(-A) public static function neg(t:IntPtr):IntPtr;

	@:op(++A) public static function preIncrement(t:IntPtr):IntPtr;
	@:op(A++) public static function postIncrement(t:IntPtr):IntPtr;
	@:op(--A) public static function preDecrement(t:IntPtr):IntPtr;
	@:op(A--) public static function postDecrement(t:IntPtr):IntPtr;
}
