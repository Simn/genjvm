package test;

interface BaseInterface {
	function test(value:String):String;
}

interface ChildInterface extends BaseInterface {
	function test2(value:String):String;
}

class Base implements BaseInterface {
	function new() {}

	public function test(value:String) {
		return "Base.test(" + value + ")";
	}
}

class Child extends Base implements ChildInterface {
	public function new() {
		super();
	}

	override function test(value:String) {
		return super.test(value) + "Main.test(" + value + ")";
	}

	public function test2(value:String) {
		return "Main.test2(" + value + ")";
	}
}

class TestOop extends BaseTest {
	public function new() {
		super();
		var child = new Child();
		eq("Base.test(MainValue)Main.test(MainValue)", child.test("MainValue"));
		eq("Base.test(MainValue)Main.test(MainValue)", (child : BaseInterface).test("MainValue"));
		eq("Main.test2(MainValue)", (child : ChildInterface).test2("MainValue"));
	}
}
