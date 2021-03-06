package unit.issues;

#if flash
private class NoProtected {}

private class Base extends NoProtected {
	public var x:Int;

	public function new() {
		x = f();
	}

	@:protected function f() return 1;
}

private class Child extends Base {
}

private class GrandChild extends Child {
	override function f() return 2;
}
#end

class Issue8248 extends unit.Test {
	#if flash
	function test() {
		eq(new GrandChild().x, 2);
	}
	#end
}
