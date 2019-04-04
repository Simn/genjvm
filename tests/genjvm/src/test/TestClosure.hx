package test;

class SomeClass<T> {
	var value:T;

	public function new(value:T) {
		this.value = value;
	}

	public function getValue() {
		return value;
	}
}

@:analyzer(ignore)
class TestClosure extends BaseTest {

	function testFieldClosure() {
		// haxe

		var someClass = new SomeClass(12);
		var closure = someClass.getValue;
		eq(12, closure());

		var closure:Dynamic = someClass.getValue;
		eq(12, closure());

		var closure:Dynamic = (someClass : Dynamic).getValue;
		eq(12, closure());

		// native

		var s:java.lang.JavaString.String = cast "foo";
		var closure = s.concat;
		eq("foobar", closure("bar"));

		var closure:Dynamic = s.concat;
		eq("foobar", closure("bar"));

		var closure:Dynamic = (s : Dynamic).concat;
		eq("foobar", closure("bar"));
	}

	function testLocalClosure() {
		var intVar = 0;
		var stringVar = "foo";

		function outerClosure(outerArg:String) {
			var outerVar = false;
			function innerClosure(innerArg:Int) {
				var innerVar = "foo";
				eq(0, intVar);
				eq("foo", stringVar);
				eq("outer arg", outerArg);
				eq(false, outerVar);
				eq(12, innerArg);
				eq("foo", innerVar);

				stringVar = "bar";
				outerArg = "set from inner";
				outerVar = true;

				eq(0, intVar);
				eq("bar", stringVar);
				eq("set from inner", outerArg);
				eq(true, outerVar);
				eq(12, innerArg);
				eq("foo", innerVar);
			}

			eq(0, intVar);
			eq("foo", stringVar);
			eq("outer arg", outerArg);
			eq(false, outerVar);

			innerClosure(12);

			eq(0, intVar);
			eq("bar", stringVar);
			eq("set from inner", outerArg);
			eq(true, outerVar);
		}
		outerClosure("outer arg");

		eq(0, intVar);
		eq("bar", stringVar);
	}

	function testTestIFoundSomewhere() {
		var funcs = [for (i in 0...5) function() return i];
		for (i in 0...5)
			eq(i, funcs[i]());
		var a = 0;
		var f1 = {
			var b = 1;
			function() {
				a = 1;
				b = 3;
				return b;
			}
		}
		var f2 = {
			var c = 2;
			function() {
				return c;
			}
		}
		eq(3, f1());
		eq(2, f2());
		eq(1, a);

		var s0 = "begin";
		function f1(s1:String) {
			var s2 = "1";
			function f2(s3:String) {
				var s4 = "2";
				function f3(s5:String) {
					return s0 + s1 + s2 + s3 + s4 + s5;
				}
				return f3;
			}
			return f2;
		}
		var s = f1("foo")("bar")("end");
		eq("beginfoo1bar2end", s);
	}

	function testRecursive() {
		var funcs = [];
		var i = 0;
		function loop() {
			var k = i++;
			funcs.push(function() return k);
			if (k < 3) {
				loop();
			}
		}
		loop();
		for (i in 0...3)
			eq(i, funcs[i]());
	}

	function testIterative() {
		var funcs = [];
		var i = 0;
		while (i < 3) {
			var k = i++;
			funcs.push(function() return k);
		}
		for (i in 0...3)
			eq(i, funcs[i]());
	}

	function testClosureOfClosureOfClosure() {
		var f = function(a) return function(b) return function(c) return a + b + c;
		eq(6, f(1)(2)(3));
	}
}
