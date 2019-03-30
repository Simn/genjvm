package test;

import haxe.PosInfos;

class BaseTest {
	static public var numTests:Int;
	static public var numFailures:Int;

	public function new() {}

	function eq<T>(expected:T, actual:T, ?p:PosInfos) {
		++numTests;
		if (expected != actual) {
			++numFailures;
			haxe.Log.trace('$actual should be $expected', p);
		}
	}

	function t(v:Bool, ?p:PosInfos) {
		eq(true, v, p);
	}

	function f(v:Bool, ?p:PosInfos) {
		eq(false, v, p);
	}
}
