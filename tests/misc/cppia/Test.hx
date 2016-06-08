class Test {

  public function new(arg1:String,arg2:Float) {
  }

  public function doTest1() {
#if cppia
    return 42;
#else
    return 1;
#end
  }

  public function doTest1WithArgs(i:Int) {
#if cppia
    return i + 1;
#else
    return i;
#end
  }

  public static function doTest() {
#if cppia
    return 43;
#else
    return 2;
#end
  }

  public static function doTestWithArgs(i:Int) {
#if cppia
    return i + 1;
#else
    return i;
#end
  }

}
