package unit.issues;

class Issue4312 extends Test
{
	public function test_1():Void
	{
		// t(true);
		// if (Std.random(2) == 0)
		// 	return null;


		return null;
	}
//
// 	public function test_2():Void
// 	{
// 		t(true);
// 		if (Std.random(2) == 0)
// 			return test();
//
//
// 		return test();
// 	}

	public function test_3():Void
	{
		t(true);
		if (Std.random(2) == 0)
		{
			t(true);
			return null;
		}

		return null;
	}
}
