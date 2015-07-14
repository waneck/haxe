import haxe.extern.Rest;

@:forward abstract NSArray<T : Id>(__NSArray<T>) from __NSArray<T> to __NSArray<T>
{
	@:extern inline public static function alloc<T : Id>()
		return __NSArray.alloc();

	@:extern inline public static function arrayWithObject<T : Id>(obj:T)
		return __NSArray.arrayWithObject(obj);
}

@:native("NSArray") @:objc extern class __NSArray<T : Id>
{
	static function alloc<T : Id>():NSArray<T>;
	static function arrayWithObject<T : Id>(obj:T):NSArray<T>;

	function init():NSArray<T>;
	@:native('initWithObjects:...') function initWithObjects(objectsTerminatingInNull:Rest<T>):NSArray<T>;
	function objectAtIndex(index:Int):T;
	var count(get,never):Int;
	@:native('count') private function get_count():Int;
}

