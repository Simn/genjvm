package test;

private class TestClass {
	public var value:String;
	public function new(value:String) {
		this.value = value;
	}
}

class TestReflectApi extends BaseTest {
	public function new() {
		super();
		testHasField();
		testField();
		testSetField();
		testFields();
	}

	function testHasField() {
		var obj = {
			x: 12,
			y: function() { },
			z: "foo"
		};
		t(Reflect.hasField(obj, "x"));
		t(Reflect.hasField(obj, "y"));
		t(Reflect.hasField(obj, "z"));
		t(!Reflect.hasField(obj, "a"));
	}

	function testField() {
		var obj = {
			x: 12,
			y: function() { return "foo"; },
			z: "foo"
		};
		eq(12, Reflect.field(obj, "x"));
		eq("foo", Reflect.field(obj, "y")());
		eq("foo", Reflect.field(obj, "z"));
		eq(null, Reflect.field(obj, "a"));

		var obj = new TestClass("foo");
		eq("foo", Reflect.field(obj, "value"));
		eq(null, Reflect.field(obj, "a"));
	}

	function testSetField() {
		var obj = {
			x: 12,
			y: function() { return "foo"; },
			z: "foo"
		};
		Reflect.setField(obj, "x", 13);
		eq(13, obj.x);

		Reflect.setField(obj, "y", function() return "bar");
		eq("bar", obj.y());

		Reflect.setField(obj, "z", "bar");
		eq("bar", obj.z);

		Reflect.setField(obj, "a", "blub");
		eq("blub", Reflect.field(obj, "a"));

		var obj = new TestClass("foo");
		Reflect.setField(obj, "value", "bar");
		eq("bar", obj.value);

		Reflect.setField(obj, "dontExist", 12);
		eq(null, Reflect.field(obj, "dontExist"));
	}

	function testFields() {
		var obj = {
			x: 12,
			y: function() { return "foo"; },
			z: "foo"
		};
		var fields = Reflect.fields(obj);
		t(fields.indexOf("x") > -1);
		t(fields.indexOf("y") > -1);
		t(fields.indexOf("z") > -1);
		t(fields.indexOf("a") == -1);
	}
}