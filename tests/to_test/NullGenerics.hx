class NullGenerics
{
	private static function main()
	{
		var x = new Z();
		trace(x.test2("a","b","c",null));
		var x2:X<String,String> = x;
		trace(x2.test2("a","b","c",null));
		var x3:X<Dynamic, Dynamic> = x;
		trace(x3.test2("a","b","c",null));
		var x4:Dynamic = x;
		trace(x4.test2("a","b","c",null));

		var a = [1,2,3,4];
		a1(a);
		trace(a);
	}

	static function a1(arr:Array<Null<Int>>)
	{
		arr.push(10);
		arr.push(null);
	}
}

//some tricky cases to test
private class X<A,B>
{
	public function new()
	{

	}

	public function test<C>(a:A, b:Null<B>, c:C, d:Null<C>):B
	{
		return b;
	}
	//on C#: test2<C>(a:A, b:B, c:C, d:haxe.Null<C>):Array<haxe.Null<B>>
	public function test2<C>(a:A, b:B, c:C, d:Null<C>):Array<Null<B>>
	{
		return [b];
	}
}

private class Y<D> extends X<Null<D>, Null<D>>
{
}

private class Z extends Y<String>
{
	override public function test2<C>(a:String, b:String, c:C, d:Null<C>):Array<String>
	{
		return [a,b];
	}
}

private class Z2 extends Y<Int>
{
}
