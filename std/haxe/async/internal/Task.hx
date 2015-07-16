package haxe.async.internal;
import haxe.async.Scheduler;

/**
	This is the internal representation of an asynchronous task. You should probably look at
	`haxe.async.Task`, which is the public-facing API
 **/
class Task<V>
{
	private var slot:SchedulerSlot;
	private var eip:Int = 0;
	private var cachedValue:V;

	public function handleError(exception:Dynamic):Bool
	{
		this.eip = -1; //error state
		return false;
	}

	public function moveNext(message:Dynamic):Bool
	{
		return false;
	}

	public function next():V
	{
		return cachedValue;
	}

	public function reset():Void
	{
		this.eip = 0;
	}
}
