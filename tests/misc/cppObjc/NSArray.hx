import haxe.extern.Rest;

@:forward abstract NSArray<T : Id>(__NSArray<T>) from __NSArray<T> to __NSArray<T>
{
	public var length(get,never):Int;

	@:extern inline private function get_length()
		return this.count;

	@:extern inline public static function alloc<T : Id>()
		return __NSArray.alloc();

	@:extern inline public static function arrayWithObject<T : Id>(obj:T)
		return __NSArray.arrayWithObject(obj);

	@:arrayAccess @:extern inline public function get(idx:Int):T
		return this.objectAtIndex(idx);

	public static function fromArrayCopy<T : Id>(arr:Array<T>)
	{
		var ret = NSMutableArray.alloc().initWithCapacity(arr.length);
		for (i in 0...arr.length)
		{
			var id:T = arr[i];
			ret[i] = id;
		}
		return ret;
	}
}

@:native("NSArray") @:objc extern class __NSArray<T : Id> extends NSObject
{
	static function alloc<T : Id>():NSArray<T>;
	static function arrayWithObject<T : Id>(obj:T):NSArray<T>;
	@:native('arrayWithObjects:count') static function arrayWithObjects<T : Id>(objects:cpp.RawConstPointer<Id>, count:cpp.UInt32):NSArray<T>;

	function init():NSArray<T>;
	@:native('initWithObjects:...') function initWithObjects(objectsTerminatingInNull:Rest<T>):NSArray<T>;
	function objectAtIndex(index:Int):T;
	var count(get,never):Int;
	@:native('count') private function get_count():Int;
}

