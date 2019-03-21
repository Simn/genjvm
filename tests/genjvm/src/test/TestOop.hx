package test;

class Base {
	function new() { }
	public function test(value:String) {
		return "Base.test(" + value + ")";
	}
}

class Child extends Base {
	public function new() {
		super();
	}

	override function test(value:String) {
		return super.test(value) + "Main.test(" + value + ")";
	}
}

class TestOop extends BaseTest {
	public function new() {
		super();
		var child = new Child();
		eq("Base.test(MainValue)Main.test(MainValue)", child.test("MainValue"));
	}
}