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
	public function new() {
		super();
		testFieldClosure();
	}

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
}
