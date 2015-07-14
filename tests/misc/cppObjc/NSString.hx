
@:forward abstract NSString(__NSString) from __NSString to __NSString
{
	@:from @:extern inline public static function fromString(str:String):NSString
		return __NSString.stringWithUTF8String(str);

	@:to @:extern inline public function toString():String
		return this.UTF8String();
}

@:native("NSString") @:objc extern class __NSString implements Id
{
	static function stringWithUTF8String(str:cpp.CastCharStar):NSString;

	function UTF8String():cpp.ConstCharStar;
}

