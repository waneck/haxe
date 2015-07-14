@:forward abstract NSMutableArray<T : Id>(__NSMutableArray<T>) from __NSMutableArray<T> to __NSMutableArray<T>
{
}

@:objc extern class __NSMutableArray<T : Id> extends _NSArray<T>
{
	static function alloc<T : Id>():NSMutableArray<T>;

	override function init():NSMutableArray<T>;

	@:native('insertObject:atIndex') function insertObject(obj:T, atIndex:Int):Void;

	function addObject(obj:T):Void;

	function removeLastObject():Void;
}

