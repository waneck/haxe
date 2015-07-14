@:forward abstract NSNumber(__NSNumber) from __NSNumber to __NSNumber
{
	@:from @:extern inline public static function fromInt(i:Int):NSNumber
		return __NSNumber.numberWithInt(i);

	@:to @:extern inline public function toInt():Int
		return this.intValue();

	@:to @:extern inline public function toBool():Bool
		return this.boolValue();
}

@:native("NSNumber") @:objc extern class __NSNumber implements Id
{
	static function numberWithInt(i:Int):NSNumber;
	function intValue():Int;
	function boolValue():Bool;
}
