package test;

import haxe.ds.Option;

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
		testIsFunction();
		testIsEnumValue();
		testDeleteField();
	}

	function testHasField() {
		var obj = {
			x: 12,
			y: function() {},
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
			y: function() {
				return "foo";
			},
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
			y: function() {
				return "foo";
			},
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
			y: function() {
				return "foo";
			},
			z: "foo"
		};
		var fields = Reflect.fields(obj);
		t(fields.indexOf("x") > -1);
		t(fields.indexOf("y") > -1);
		t(fields.indexOf("z") > -1);
		t(fields.indexOf("a") == -1);
	}

	function testIsFunction() {
		t(Reflect.isFunction(testIsFunction));
		t(Reflect.isFunction(Reflect.isFunction));
		t(Reflect.isFunction(function() {}));
		f(Reflect.isFunction("foo"));
		f(Reflect.isFunction(null));
		f(Reflect.isFunction((null : java.lang.invoke.MethodHandle)));
	}

	function testIsEnumValue() {
		t(Reflect.isEnumValue(None));
		t(Reflect.isEnumValue(Some(12)));
		f(Reflect.isEnumValue("foo"));
		// f(Reflect.isEnumValue(Some)); // TODO
		f(Reflect.isEnumValue(null));
		f(Reflect.isEnumValue(Option));
	}

	function testDeleteField() {
		var obj = {
			x: 12,
			y: function() {
				return "foo";
			},
			z: "foo"
		};
		t(Reflect.deleteField(obj, "x"));
		f(Reflect.deleteField(obj, "x"));
		f(Reflect.hasField(obj, "x"));
		eq(0, obj.x);
		obj.x = 12;
		t(Reflect.hasField(obj, "x"));
		t(Reflect.deleteField(obj, "x"));
		f(Reflect.deleteField(obj, "x"));

		t(Reflect.deleteField(obj, "z"));
		f(Reflect.deleteField(obj, "z"));
		f(Reflect.hasField(obj, "z"));
		eq(null, obj.z);
		(obj : Dynamic).z = "bar";
		t(Reflect.hasField(obj, "z"));
		t(Reflect.deleteField(obj, "z"));
		f(Reflect.deleteField(obj, "z"));

		Reflect.setField(obj, "z", "bar");
		eq("bar", obj.z);
		eq("bar", Reflect.field(obj, "z"));
		t(Reflect.hasField(obj, "z"));
		t(Reflect.deleteField(obj, "z"));
		f(Reflect.deleteField(obj, "z"));
	}
}
