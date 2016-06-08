class TestCppia extends haxe.unit.TestCase
{
	static function main()
	{
		var x:TestCppia = null;
		var runner = new haxe.unit.TestRunner();
		runner.add(new TestCppia());
		var code = runner.run() ? 0 : 1;
		Sys.exit(code);
	}

	public function test()
	{
		var t = new Test("test",1);
		assertEquals(t.doTest1(), 1);
		assertEquals(t.doTest1WithArgs(10), 10);
		var t2 = Type.createInstance(Type.resolveClass("Test"), ["",1]);
		assertEquals(t2.doTest1(), 1);
		assertEquals(t2.doTest1WithArgs(10), 10);
		var t3:Test = Type.createInstance(Type.resolveClass("Test"), ["",1]);
		assertEquals(t3.doTest1(), 1);
		assertEquals(t3.doTest1WithArgs(10), 10);
		assertEquals(Test.doTest(), 2);
		assertEquals(Test.doTestWithArgs(10), 10);
		var dyn:Dynamic = Type.resolveClass("Test");
		assertEquals(dyn.doTest(), 2);
		assertEquals(dyn.doTestWithArgs(10), 10);

		// load cppia
		trace('loading cppia');
		untyped __global__.__scriptable_load_cppia(sys.io.File.getContent("bin/cppia.cppia"));
		var t = new Test("test",1);
		assertEquals(t.doTest1(), 42);
		assertEquals(t.doTest1WithArgs(10), 11);
		var t2 = Type.createInstance(Type.resolveClass("Test"), ["",1]);
		assertEquals(t2.doTest1(), 42);
		assertEquals(t2.doTest1WithArgs(10), 11);
		var t3:Test = Type.createInstance(Type.resolveClass("Test"), ["",1]);
		assertEquals(t3.doTest1(), 42);
		assertEquals(t3.doTest1WithArgs(10), 11);
		assertEquals(Test.doTest(), 43);
		assertEquals(Test.doTestWithArgs(10), 11);
		var dyn:Dynamic = Type.resolveClass("Test");
		assertEquals(dyn.doTest(), 43);
		assertEquals(dyn.doTestWithArgs(10), 11);

	}
}
