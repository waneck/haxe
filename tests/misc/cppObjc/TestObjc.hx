class TestObjc extends haxe.unit.TestCase
{
	static function main()
	{
		var runner = new haxe.unit.TestRunner();
		runner.add(new TestObjc());
		var code = runner.run() ? 0 : 1;

		for (_ in 0...2) // run twice to make sure all finalizers are called
			cpp.vm.Gc.run(true);

		var hasLeaks = Sys.command('leaks',['TestObjc' #if debug + '-debug' #end]);
		if (hasLeaks != 0)
			code = hasLeaks;
		Sys.exit(code);
	}

	var cls:TestClass;

	public function testCall()
	{
		assertEquals(TestClass.aStatic(), 42);
		cls = TestClass.alloc().init();
		assertEquals(cls.getOtherThing(), 0);
		cls.setOtherThing(42);
		assertEquals(cls.otherThing, 42);
		assertEquals(cls.getOtherThing(), 42);
		assertEquals(cls.getOtherThingChar(), 42);
		assertEquals(cls.isBiggerThan10(2), false);
		assertEquals(cls.isBiggerThan10(12), true);
		assertEquals(cls.isBiggerThan10Int(3), false);
		assertEquals(cls.isBiggerThan10Int(14), true);
		assertEquals(cls.isBiggerThan10Num(3).boolValue(), false);
		assertEquals(cls.isBiggerThan10Num(14).boolValue(), true);
		assertEquals(cls.addHello("World"), "Hello, World");
		cls.something = " test";
		assertEquals(cls.something, " test");
		assertEquals(cls.addSomething("Hey,"), "Hey, test");
		assertEquals(cls.addHelloAndString("World"," it works"), "Hello, World it works");
		cls.release();
	}

	public function testVar()
	{
		cls = TestClass.alloc().init();
		cls.setOtherThing(142);
		assertEquals(cls.otherThing, 142);
		assertEquals(cls.getOtherThing(), 142);
		cls.release();
	}

	public function testBoxing()
	{
		cls = TestClass.alloc().init();

		cls.setOtherThing(255);

		var dyn:Dynamic = cls;
		this.assertTrue(dyn != null);
		this.assertTrue(cls != null);

		var someObjDecl = { a:10, b: cls };
		dyn = someObjDecl; // don't let Haxe inline that TObjectDecl
		assertEquals(someObjDecl.b.getOtherThing(), 255);
		assertEquals(getFieldB(someObjDecl).getOtherThing(), 255);
		cls = someObjDecl.b;
		assertTrue(someObjDecl.b == cls);
		dyn = cls;

		cls.release();
		cls = null;
		this.assertTrue(cls == null);
		cls = dyn;

		assertEquals(cls.getOtherThing(), 255);
		cls = null;
		dyn = null;
		dyn = cls;
		assertTrue(dyn == null);
		assertEquals(dyn,null);
		assertTrue(dyn == cls);
		assertEquals(dyn,cls);
		cls.release();
	}

	static function getFieldB<T>(d:{ a:Int, b: T }):T
		return d.b;

	public function testNull()
	{
		this.assertTrue(TestClass.isNull(null));
		var cls = TestClass.alloc().init();
		this.assertFalse(TestClass.isNull(cls));
		cls.release();
	}

	public function testInterface()
	{
		cls = TestClass.alloc().init();
		cls.setOtherThing(21);
		this.assertTrue(cls.getSelf() == cls);
		this.assertEquals(cls.getSelf(), cls);

		var iface:TestInterface = cls;
		var obj:Dynamic = iface;
		this.assertTrue(iface == cls);
		this.assertEquals(iface, cls);
		this.assertTrue(obj == cls);
		this.assertEquals(obj, cls);
		this.assertEquals(iface.getSelf(), cls);
		this.assertEquals(iface.getSelf(), cls.getSelf());

		this.assertEquals(iface.getOtherThing(), 21);
		this.assertEquals(iface.getOtherThingChar(), 21);
		cls.setOtherThing(100);
		this.assertEquals(iface.getOtherThing(), 100);
		this.assertEquals(iface.getOtherThingChar(), 100);

		this.assertEquals("someOptionalMethod!",iface.someOptionalMethod());

		cls.release();
	}

	public function testArray()
	{
		var arr:NSArray<NSNumber> = (NSArray.alloc() : NSArray<NSNumber>).initWithObjects(1,2,3,null);
		assertEquals(arr.count, 3);
		assertEquals(arr[0], 1);
		assertEquals(arr[1], 2);
		assertEquals(arr[2], 3);
		arr.release();

		var arr = NSArray.fromArrayCopy([TestClass.alloc().init(), TestClass.alloc().init()]);
		assertEquals(arr.count,2);
		arr[0].setOtherThing(1000);
		assertEquals(arr[0].getOtherThing(), 1000);
		assertEquals(arr[1].getOtherThing(), 0);

		for (v in arr) v.release(); // need to release because we've used alloc().init()
		arr.release();
	}
}

@:include("./native/include/test.h")
@:objc extern interface TestInterface extends Id
{
	function getSelf():TestInterface;
	function getOtherThing():Int;
	function getOtherThingChar():cpp.Int8;

	@:optional function someOptionalMethod():NSString;
	@:optional function unimplementedOptional():NSString;
}

@:include("./native/include/test.h")
@:sourceFile("./native/test.m")
@:objc extern class TestClass implements TestInterface implements Id
{
	static function aStatic():Int;
	static function isNull(t:TestClass):Bool;

	static function alloc():TestClass;
	function init():TestClass;

	var something(get,set):NSString;
	var otherThing:Int;

	@:native("something") private function get_something():NSString;
	@:native("setSomething") private function set_something(value:NSString):NSString;

	function setOtherThing(value:Int):Void;
	function getOtherThing():Int;
	function getOtherThingChar():cpp.Int8;
	function addHello(str:NSString):NSString;
	@:native("addHello:andString") function addHelloAndString(str:NSString, str2:NSString):NSString;
	function addSomething(str:NSString):NSString;
	function isBiggerThan10(value:NSNumber):Bool;
	function isBiggerThan10Num(value:NSNumber):NSNumber;
	function isBiggerThan10Int(integer:Int):Bool;

	function release():Void;
	function retainCount():Int;

	function getSelf():TestClass;

	function someOptionalMethod():NSString;

	@:deprecated('This method is not implemented on this class')
	@:noCompletion @:optional function unimplementedOptional():NSString;

	@:plain static function some_c_call(t:TestClass):Int;
	@:plain static function is_bigger_than_10(t:TestClass, val:Int):Bool;
}
