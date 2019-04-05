package test;

import haxe.ds.IntMap;
import haxe.ds.StringMap;

private class ChaosConstructor {
	public var value:Int;

	public function new(value:Int) {
		this.value = value;
	}
}

private class ChaosConstructorChild extends ChaosConstructor {
	public function new(value:Int) {
		super(value > 0 ? 1 : 0);
	}
}

@:analyzer(ignore)
class TestChaos extends BaseTest {
	var intField:Int;
	var intFieldDynamic:Dynamic;

	static var NaN = java.lang.Double.DoubleClass.NaN;
	static var staticIntField:Int;
	static var staticIntFieldDynamic:Dynamic;
	static var trueValue = true;
	static var falseValue = false;

	function testAssignment() {
		// var
		var a = 1;
		eq(1, a);
		a = 2;
		eq(2, a);
		a += 1;
		eq(3, a);
		eq(3, a++);
		eq(4, a);
		eq(5, ++a);

		// instance field
		intField = 1;
		eq(1, intField);
		intField = 2;
		eq(2, intField);
		intField += 1;
		eq(3, intField);
		eq(3, intField++);
		eq(4, intField);
		eq(5, ++intField);

		// static field
		staticIntField = 1;
		eq(1, staticIntField);
		staticIntField = 2;
		eq(2, staticIntField);
		staticIntField += 1;
		eq(3, staticIntField);
		eq(3, staticIntField++);
		eq(4, staticIntField);
		eq(5, ++staticIntField);
	}

	function testAssignmentDynamic() {
		// var
		var a:Dynamic = 1;
		eq(1, a);
		a = 2;
		eq(2, a);
		a += 1;
		eq(3, a);
		eq(3., a++);
		eq(4, a);
		eq(5., ++a);

		// instance field
		intFieldDynamic = 1;
		eq(1, intFieldDynamic);
		intFieldDynamic = 2;
		eq(2, intFieldDynamic);
		intFieldDynamic += 1;
		eq(3, intFieldDynamic);
		eq(3., intFieldDynamic++);
		eq(4, intFieldDynamic);
		eq(5., ++intFieldDynamic);

		// static field
		staticIntFieldDynamic = 1;
		eq(1, staticIntFieldDynamic);
		staticIntFieldDynamic = 2;
		eq(2, staticIntFieldDynamic);
		staticIntFieldDynamic += 1;
		eq(3, staticIntFieldDynamic);
		eq(3., staticIntFieldDynamic++);
		eq(4, staticIntFieldDynamic);
		eq(5., ++staticIntFieldDynamic);
	}

	function testOps() {
		var a = 10;
		// arithmetic
		eq(9, a - 1);
		eq(20, a * 2);
		eq(5., a / 2); // careful with Float comparison...
		eq(1, a % 3);

		// bit
		eq(20, a << 1);
		eq(5, a >> 1);
		eq(5, a >>> 1);
		eq(10, a & 15);
		eq(15, a | 15);
		eq(2, a ^ 8);

		// unary
		eq(-10, -a);
		eq(-11, ~a);

		// boolean
		var b = true;
		eq(false, !b);
		eq(false, b && falseValue);
		eq(true, b && trueValue);
		eq(true, b || falseValue);
		eq(true, b || trueValue);

		b = false;
		eq(true, !b);
		eq(false, b && falseValue);
		eq(false, b && trueValue);
		eq(false, b || falseValue);
		eq(true, b || trueValue);

		eq(true, a > 5);
		eq(true, a >= 5);
		eq(false, a < 5);
		eq(false, a <= 5);
		eq(true, a != 5);
		eq(false, a != 10);

		eq(false, 0 > a);
		eq(false, 0 >= a);
		eq(true, 0 < a);
		eq(true, 0 <= a);
		eq(true, 0 != a);
		eq(false, 0 == a);

		var minusA = -10;
		eq(true, 0 > minusA);
		eq(true, 0 >= minusA);
		eq(false, 0 < minusA);
		eq(false, 0 <= minusA);
		eq(true, 0 != minusA);
		eq(false, 0 == minusA);
	}

	function testNullOps() {
		var a:Null<Int> = 10;
		// arithmetic
		eq(9, a - 1);
		eq(20, a * 2);
		eq(5., a / 2); // careful with Float comparison...
		eq(1, a % 3);

		// bit
		eq(20, a << 1);
		eq(5, a >> 1);
		eq(5, a >>> 1);
		eq(10, a & 15);
		eq(15, a | 15);
		eq(2, a ^ 8);

		// unary
		eq(-10, -a);
		eq(-11, ~a);

		// boolean
		var b:Null<Bool> = true;
		eq(false, !b);
		eq(false, b && falseValue);
		eq(true, b && trueValue);
		eq(true, b || falseValue);
		eq(true, b || trueValue);

		b = false;
		eq(true, !b);
		eq(false, b && falseValue);
		eq(false, b && trueValue);
		eq(false, b || falseValue);
		eq(true, b || trueValue);

		eq(true, a > 5);
		eq(true, a >= 5);
		eq(false, a < 5);
		eq(false, a <= 5);
		eq(true, a != 5);
		eq(false, a != 10);

		eq(false, 0 > a);
		eq(false, 0 >= a);
		eq(true, 0 < a);
		eq(true, 0 <= a);
		eq(true, 0 != a);
		eq(false, 0 == a);

		var minusA:Null<Int> = -10;
		eq(true, 0 > minusA);
		eq(true, 0 >= minusA);
		eq(false, 0 < minusA);
		eq(false, 0 <= minusA);
		eq(true, 0 != minusA);
		eq(false, 0 == minusA);
	}

	function testNadakoOps() {
		// bool
		var nullBool:Null<Bool> = null;

		t(null == nullBool);
		t(nullBool == null);
		f(false == nullBool);
		f(nullBool == false);
		t(false != nullBool);
		t(nullBool != false);

		// int
		var nullInt:Null<Int> = null;

		t(null == nullInt);
		t(nullInt == null);
		f(0 == nullInt);
		f(nullInt == 0);
		t(0 != nullInt);
		t(nullInt != 0);

		f(0 > nullInt);
		f(0 >= nullInt);
		f(0 < nullInt);
		f(0 <= nullInt);

		f(nullInt > 0);
		f(nullInt >= 0);
		f(nullInt < 0);
		f(nullInt <= 0);

		f(1 > nullInt);
		f(1 >= nullInt);
		f(1 < nullInt);
		f(1 <= nullInt);

		f(nullInt > 1);
		f(nullInt >= 1);
		f(nullInt < 1);
		f(nullInt <= 1);

		f(-1 > nullInt);
		f(-1 >= nullInt);
		f(-1 < nullInt);
		f(-1 <= nullInt);

		f(nullInt > -1);
		f(nullInt >= -1);
		f(nullInt < -1);
		f(nullInt <= -1);

		// // float
		var nullFloat:Null<Float> = null;

		t(null == nullFloat);
		t(nullFloat == null);
		f(0. == nullFloat);
		f(nullFloat == 0.);
		t(0. != nullFloat);
		t(nullFloat != 0.);

		f(0. > nullFloat);
		f(0. >= nullFloat);
		f(0. < nullFloat);
		f(0. <= nullFloat);

		f(nullFloat > 0.);
		f(nullFloat >= 0.);
		f(nullFloat < 0.);
		f(nullFloat <= 0.);

		f(1. > nullFloat);
		f(1. >= nullFloat);
		f(1. < nullFloat);
		f(1. <= nullFloat);

		f(nullFloat > 1.);
		f(nullFloat >= 1.);
		f(nullFloat < 1.);
		f(nullFloat <= 1.);

		f(-1. > nullFloat);
		f(-1. >= nullFloat);
		f(-1. < nullFloat);
		f(-1. <= nullFloat);

		f(nullFloat > -1.);
		f(nullFloat >= -1.);
		f(nullFloat < -1.);
		f(nullFloat <= -1.);
	}


	function testDynamicOps() {
		var a:Dynamic = 10;
		// arithmetic
		eq(9., a - 1);
		eq(20., a * 2);
		eq(5., a / 2); // careful with Float comparison...
		eq(1., a % 3);

		// bit
		eq(20, a << 1);
		eq(5, a >> 1);
		eq(5, a >>> 1);
		eq(10, a & 15);
		eq(15, a | 15);
		eq(2, a ^ 8);

		// unary
		eq(-10., -a);
		eq(-11, ~a);

		// boolean
		var b:Dynamic = true;
		eq(false, !b);
		eq(false, b && falseValue);
		eq(true, b && trueValue);
		eq(true, b || falseValue);
		eq(true, b || trueValue);

		b = false;
		eq(true, !b);
		eq(false, b && falseValue);
		eq(false, b && trueValue);
		eq(false, b || falseValue);
		eq(true, b || trueValue);

		eq(true, a > 5);
		eq(true, a >= 5);
		eq(false, a < 5);
		eq(false, a <= 5);
		eq(true, a != 5);
		eq(false, a != 10);
	}

	function testIntArray() {
		var a = [];
		eq(0, a.length);
		a.push(1);
		eq(1, a.length);
		eq(1, a.pop());
		eq(0, a.length);

		a[2] = 2;
		eq(3, a.length);
		eq(0, a[0]);
		eq(0, a[1]);
		eq(2, a[2]);

		a = [1, 2];
		var b = a.concat([3, 4]);
		eq(4, b.length);
		eq(1, b[0]);
		eq(2, b[1]);
		eq(3, b[2]);
		eq(4, b[3]);

		b.reverse();
		eq(4, b.length);
		eq(4, b[0]);
		eq(3, b[1]);
		eq(2, b[2]);
		eq(1, b[3]);

		eq(4, b.shift());
		eq(3, b.length);

		var c = [1, 2, 3, 4].slice(1, 3);
		eq(2, c.length);
		eq(2, c[0]);
		eq(3, c[1]);

		var d = [1, 2, 3, 4];
		var e = d.splice(1, 2);
		eq(2, d.length);
		eq(2, e.length);
		eq(1, d[0]);
		eq(4, d[1]);
		eq(2, e[0]);
		eq(3, e[1]);

		var f = [1, 2, 3];
		f.unshift(4);
		eq(4, f.length);
		eq(4, f[0]);

		f.insert(2, 5);
		eq(5, f.length);
		eq(4, f[0]);
		eq(1, f[1]);
		eq(5, f[2]);
		eq(2, f[3]);
		eq(3, f[4]);

		eq(false, f.remove(99));
		eq(true, f.remove(5));
		eq(4, f.length);
		eq(4, f[0]);
		eq(1, f[1]);
		eq(2, f[2]);
		eq(3, f[3]);

		eq(2, f.indexOf(2));
		eq(2, f.lastIndexOf(2));

		var g = f.copy();
		eq(4, g.length);
		eq(4, g[0]);
		eq(1, g[1]);
		eq(2, g[2]);
		eq(3, g[3]);

		var h = g.map(i -> i * 2);
		eq(4, h.length);
		eq(8, h[0]);
		eq(2, h[1]);
		eq(4, h[2]);
		eq(6, h[3]);

		var i = g.filter(i -> i & 1 == 0);
		eq(2, i.length);
		eq(4, i[0]);
		eq(2, i[1]);

		h.resize(2);
		eq(2, h.length);
		eq(8, h[0]);
		eq(2, h[1]);

		var offset = 0;
		for (i in h) {
			eq(h[offset++], i);
		}

		var j = [9, 3, 6, 2, 8, 1];
		j.sort((a, b) -> a - b);
		eq(1, j[0]);
		eq(2, j[1]);
		eq(3, j[2]);
		eq(6, j[3]);
		eq(8, j[4]);
		eq(9, j[5]);

		// TODO: toString and join
	}

	function testDynamicArray() {
		var x:Dynamic = [-101.5];
		x[0] %= 100;
		eq(-1.5, x[0]);

		var x:Dynamic = [-101.5];
		eq(-1.5, x[0] %= 100);
		eq(-1.5, x[0]);
	}

	function testStringMap() {
		var sm = new StringMap();
		eq(null, sm.get("foo"));
		eq(false, sm.exists("foo"));
		sm.set("foo", 12);
		eq(true, sm.exists("foo"));
		eq(12, sm.get("foo"));
		sm.remove("foo");
		eq(null, sm.get("foo"));

		var sm = new StringMap();
		sm.set("foo", 12);
		sm.set("bar", 13);
		var keys = [];
		var values = [];
		for (key => value in sm) {
			keys.push(key);
			values.push(value);
		}
		keys.sort(Reflect.compare);
		values.sort(Reflect.compare);
		eq(2, keys.length);
		eq(2, values.length);
		eq("bar", keys[0]);
		eq("foo", keys[1]);
		eq(12, values[0]);
		eq(13, values[1]);
	}

	function testIntMap() {
		var sm = new IntMap();
		eq(null, sm.get(12));
		eq(false, sm.exists(12));
		sm.set(12, 12);
		eq(true, sm.exists(12));
		eq(12, sm.get(12));
		sm.remove(12);
		eq(null, sm.get(12));

		var sm = new IntMap();
		sm.set(12, 12);
		sm.set(13, 13);
		var keys = [];
		var values = [];
		for (key => value in sm) {
			keys.push(key);
			values.push(value);
		}
		keys.sort(Reflect.compare);
		values.sort(Reflect.compare);
		eq(2, keys.length);
		eq(2, values.length);
		eq(12, keys[0]);
		eq(13, keys[1]);
		eq(12, values[0]);
		eq(13, values[1]);
	}

	function testObjectDecl() {
		var obj = {
			a: null,
			b: 1,
			c: "foo"
		}
		eq(obj, obj); // to avoid inline constructor
		eq(null, obj.a);
		eq(1, obj.b);
		eq("foo", obj.c);
		eq("bar", obj.a = "bar");
		eq(2, obj.b = 2);
		eq("fooo", obj.c = "fooo");

		var objDynamic:Dynamic = obj;
		eq("bar", objDynamic.a);
		eq(2, objDynamic.b);
		eq("fooo", objDynamic.c);

		eq(null, objDynamic.a = null);
		eq(1, objDynamic.b = 1);
		eq("foo", objDynamic.c = "foo");

		eq(null, objDynamic.d);
		eq("wow", objDynamic.d = "wow");
		eq("wow", objDynamic.d);
	}

	function testStringConcat() {
		eq("abc", "ab" + "c");
		eq("abc", "a" + "b" + "c");
		eq("anull", "a" + null);
		eq("nulla", null + "a");
		eq("a1", "a" + 1);
		eq("1a", 1 + "a");
		eq("atrue", "a" + true);
		eq("truea", true + "a");
		eq("a1.0", "a" + 1.0);
		eq("1.0a", 1.0 + "a");
	}

	function testBranchingCtorArgs() {
		eq(1, new ChaosConstructor(trueValue ? 1 : 0).value);

		eq(1, new ChaosConstructorChild(1).value);
		eq(0, new ChaosConstructorChild(-1).value);
	}

	function testNaN() {
		f(NaN == NaN);
		f(NaN >= NaN);
		f(NaN > NaN);
		f(NaN <= NaN);
		f(NaN < NaN);
		t(NaN != NaN);
	}

	function testNativeArray() {
		var a = new java.NativeArray(1);
		a[0] = new java.NativeArray(1);
		eq(0, a[0][0]);
		eq(12, a[0][0] = 12);
		eq(12, a[0][0]);

		var a = new java.NativeArray(1);
		a[0] = new java.NativeArray(1);
		eq(null, a[0][0]);
		eq("foo", a[0][0] = "foo");
		eq("foo", a[0][0]);
	}
}