package test;

import utest.Assert;
import utest.ITest;
import haxe.PosInfos;

class BaseTest implements ITest {
	public function new() {}

	function eq<T>(expected:T, actual:T, ?p:PosInfos) {
		Assert.equals(expected, actual, p);
	}

	function t(v:Bool, ?p:PosInfos) {
		Assert.isTrue(v, p);
	}

	function f(v:Bool, ?p:PosInfos) {
		Assert.isFalse(v, p);
	}
}
