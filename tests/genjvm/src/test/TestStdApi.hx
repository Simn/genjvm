package test;

private class MyClass {
	public function new() {}
}

private class MySubClass extends MyClass {}

private enum MyEnum {
	A;
	B;
	C(a:Int, b:String);
	D(e:MyEnum);
}

private interface MyInterface {}

class TestStdApi extends BaseTest {
	static var TYPES:Array<Dynamic> = [
		null, Int, String, Bool, Float, Array, haxe.ds.StringMap, List, Date, Xml, Math, MyEnum, MyClass, MySubClass, Class, Enum, Dynamic, MyInterface
	];
	static var TNAMES = [
		"null", "Int", "String", "Bool", "Float", "Array", "haxe.ds.StringMap", "haxe.ds.List", "Date", "Xml", "Math", "unit", "MyEnum", "unit", "MyClass",
		"unit", "MySubClass", "Class", "Enum", "Dynamic", "unit", "MyInterface"
	];

	function is(v:Dynamic, t1:Dynamic, ?t2:Dynamic, ?pos:haxe.PosInfos) {
		for (i in 0...TYPES.length) {
			var c:Dynamic = TYPES[i];
			var wasTrue = Std.is(v, c) == (c != null && (c == t1 || c == t2) || (c == Dynamic));

			if (!wasTrue) {
				trace(v, c);
			}
			t(wasTrue);
		}
		t((v is Dynamic), pos);
	}

	function testIs() {
		is(0, Int, Float);
		is(1, Int, Float);
		is(-1, Int, Float);
		is(2.0, Int, Float);
		is(1.2, Float);
		is(1e10, Float);
		is(-1e10, Float);
		// TODO: needs rerouting
		// is(Math.NaN, Float);
		// is(Math.POSITIVE_INFINITY, Float);
		// is(Math.NEGATIVE_INFINITY, Float);
		is(true, Bool);
		is(false, Bool);
		is("Hello", String);
		is("123", String);
		is("false", String);
		is("", String);
		is([], Array);
		is([1, 2], Array);
		is([1.1, 2.2], Array);
		is(["a", "b"], Array);
		is((["a", 2] : Array<Dynamic>), Array);
		is(new List(), List);
		is(new haxe.ds.StringMap(), haxe.ds.StringMap);
		is(new MyClass(), MyClass);
		is(new MySubClass(), MyClass, MySubClass);
		is(MyEnum.A, MyEnum);
		is(MyEnum.C(0, ""), MyEnum);
		is(Date.now(), Date);
		is({x: 0}, null);
		is(function() {}, null);
		is(MyClass, Class);
		is(MyEnum, Enum);
	}

	function testParse() {
		eq(Std.parseInt("65"), 65);
		eq(Std.parseInt("65.3"), 65);
		eq(Std.parseInt("65.7"), 65);
		eq(Std.parseInt("65.7a"), 65);
		eq(Std.parseFloat("65"), 65.0);
		eq(Std.parseFloat("65a"), 65.0);
		eq(Std.parseFloat("65.3"), 65.3);
		eq(Std.parseFloat("-1e10"), -1e10);
		eq(Std.parseFloat("-1E10"), -1e10);
		eq(Std.parseInt("0xFF"), 255);
		eq(Std.parseInt("0xFF"), 255);
	}
}
