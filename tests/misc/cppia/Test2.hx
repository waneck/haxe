class Test2 extends Test {
  override public function doTest1() {
#if cppia
    return 44;
#else
    throw 'assert';
#end
  }
}
