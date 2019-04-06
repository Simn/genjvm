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

interface TPInterface0<T> {
	public function get(t:T):T;
}

class TPClass0 implements TPInterface0<String> {
	public function new() { }

	public function get(s:String) {
		return s;
	}
}

interface TPInterface1<T> extends TPInterface0<T> { }

class TPClass1 implements TPInterface1<String> {
	public function new() { }

	public function get(s:String) {
		return s;
	}
}

interface TPInterface2 extends TPInterface0<String> { }

class TPClass2 implements TPInterface2 {
	public function new() { }

	public function get(s:String) {
		return s;
	}
}

class TPBaseClass0<T> {
	public function new() { }
	public function get(t:T) {
		return "TPBaseClass.get(T): " + t;
	}
}

class TPChildClass0 extends TPBaseClass0<String> {
	override function get(t:String) {
		return super.get(t) + "TPChildClass.get(String): " + t;
	}
}

class TPChildClass1 extends TPBaseClass0<Int> { }

class TPChildClass2 extends TPChildClass1 {
	override function get(t:Int) {
		return super.get(t) + "TPChildClass.get(Int): " + t;
	}
}

class TPChildClass3<T> extends TPBaseClass0<T> { }

class TPChildClass4 extends TPChildClass3<Int> {
	override function get(t:Int) {
		return super.get(t) + "TPChildClass.get(Int): " + t;
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

	function testTypeParams() {
		var i:TPInterface0<String> = new TPClass0();
		eq("foo", i.get("foo"));

		var i:TPInterface1<String> = new TPClass1();
		eq("foo", i.get("foo"));

		var i:TPInterface2 = new TPClass2();
		eq("foo", i.get("foo"));

		var base:TPBaseClass0<String> = new TPChildClass0();
		eq("TPBaseClass.get(T): fooTPChildClass.get(String): foo", base.get("foo"));

		var base:TPBaseClass0<Int> = new TPChildClass2();
		eq("TPBaseClass.get(T): 12TPChildClass.get(Int): 12", base.get(12));

		var base:TPBaseClass0<Int> = new TPChildClass4();
		eq("TPBaseClass.get(T): 12TPChildClass.get(Int): 12", base.get(12));
	}
}
