package haxe.async;

class Scheduler
{
}

@:enum abstract TaskReturnState(Int)
{
	var Removed = 0;
	var Paused = 1;
	var DependsOn = 2;
	var HasValue = 3;
}
