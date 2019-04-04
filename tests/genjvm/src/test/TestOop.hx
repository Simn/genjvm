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
	public var field:String;

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

class HxGenBase {
	public var value:String;

	@:hxGen function new() {
		value = "HxGenBase.new()V";
	}
}

class HxGenChild extends HxGenBase {
	public function new() {
		super();
		value += "HxGenChild.new()V";
	}
}

class ThisBeforeSuperBase {
	public var value:String;

	@:hxGen function new() {
		value += "ThisBeforeSuperBase.new()V";
	}
}

class ThisBeforeSuperChild extends ThisBeforeSuperBase {
	public function new() {
		value = "ThisBeforeSuperChild.new()V";
		super();
	}
}

class PassThroughParent {
	public var value:String;

    public function new() {
		value = "PassThroughParent.new()V";
	}
}

class PassThroughCurrent extends PassThroughParent {

}

class PassThroughChild extends PassThroughCurrent {
    public function new() {
        super();
		value += "PassThroughChild.new()V";
    }
}

class FieldInitParent {
    public function new() { }
}

class FieldInitCurrent extends FieldInitParent {
    public var hasInit = true;
}

class FieldInitChild extends FieldInitCurrent {
    public function new() {
        super();
    }
}

class HxGenFieldInitParent {
    @:hxGen public function new() { }
}

class HxGenFieldInitCurrent extends HxGenFieldInitParent {
    public var hasInit = true;
}

class HxGenFieldInitChild extends HxGenFieldInitCurrent {
    public function new() {
        super();
    }
}

typedef TestStructure = {
	function test(value:String):String;
}

typedef FieldStructure = {
	var field:String;
}

class TestOop extends BaseTest {
	function testBasic() {
		var child = new Child();
		eq("Base.test(MainValue)Main.test(MainValue)", child.test("MainValue"));
		eq("Base.test(MainValue)Main.test(MainValue)", (child : BaseInterface).test("MainValue"));
		eq("Main.test2(MainValue)", (child : ChildInterface).test2("MainValue"));

		function subtype(obj:TestStructure, value:String) {
			return obj.test(value);
		}
		eq("Base.test(MainValue)Main.test(MainValue)", subtype(child, "MainValue"));

		function subtype(obj:FieldStructure) {
			return obj.field;
		}
		child.field = "FieldValue";
		eq("FieldValue", subtype(child));
	}

	function testSpecialtors() {
		var c = new HxGenChild();
		eq("HxGenBase.new()VHxGenChild.new()V", c.value);

		var c = new ThisBeforeSuperChild();
		eq("ThisBeforeSuperChild.new()VThisBeforeSuperBase.new()V", c.value);

		var c = new PassThroughChild();
		eq("PassThroughParent.new()VPassThroughChild.new()V", c.value);

		var c = new FieldInitChild();
		t(c.hasInit);

		var c = new HxGenFieldInitChild();
		t(c.hasInit);
	}
}
