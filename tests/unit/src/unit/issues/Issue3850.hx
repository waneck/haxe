package unit.issues;

class Issue3850 extends Test
{
	public function test()
	{
		var a:Array<A> = [];
		eq(fn(a),null);
		var b:Array<{ x:A }> = [];
		eq(fn(a),null);
		var b:Array<{ x:Array<A> }> = [];
		eq(fn(a),null);
		var b:Array<{ x:Array<{ y:A }> }> = [];
		eq(fn(a),null);
	}

	static function fn<T>(o:Array<T>):T return o[0];
}

private abstract A(String) {}
