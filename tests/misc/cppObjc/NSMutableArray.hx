@:forward abstract NSMutableArray<T : Id>(__NSMutableArray<T>) from __NSMutableArray<T> to __NSMutableArray<T> to NSArray<T>
{
	public var length(get,never):Int;

	@:extern inline private function get_length()
		return this.count;

	@:extern inline public static function alloc<T : Id>()
		return __NSMutableArray.alloc();

	@:arrayAccess @:extern inline public function get(idx:Int):T
		return this.objectAtIndex(idx);

	@:arrayAccess @:extern inline public function set(idx:Int, val:T):Void
		this.insertObject(val, idx);
}

@:native("NSMutableArray") @:objc extern class __NSMutableArray<T : Id> extends NSArray.__NSArray<T>
{
	static function alloc<T : Id>():NSMutableArray<T>;

	override function init():NSMutableArray<T>;
	function initWithCapacity(numItems:cpp.UInt32):NSMutableArray<T>;

	@:native('insertObject:atIndex') function insertObject(obj:T, atIndex:Int):Void;

	function addObject(obj:T):Void;

	function removeLastObject():Void;
}

