package haxe.async.internal;

class SchedulerSlot
{
	public var scheduler(default,null):Scheduler;
	private var asyncCallChain:Array<Task<Dynamic>>;

	public function new(scheduler)
	{
		this.scheduler = scheduler;
		this.asyncCallChain = [];
	}

	private function handleError(exception:Dynamic)
	{
		var callChain = asyncCallChain;
		var len = callChain.length,
		    i = len;
		while ( i --> 0 )
		{
			if (callChain[i].handleError(exception))
			{
				if (i + 1 != len)
					callChain.splice(i+1,len-i);
				break;
			}
		}
		// no handler


	}
}
