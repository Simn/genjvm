package test;

private class SomeClass {
	public function new() {}
}

private class SomeClassWithArgs {
	var value:String;

	public function new(s:String) {
		value = s;
	}
}

class TestTypeApi extends BaseTest {
	public function new() {
		super();
		testGetClass();
		testGetSuperClass();
		testGetClassName();
		testResolveClass();
	}

	function testGetClass() {
		eq(null, Type.getClass(null));
		eq(null, Type.getClass(TestTypeApi));
		eq(null, Type.getClass(java.lang.Object));
		eq(cast SomeClass, Type.getClass(new SomeClass()));
		eq(cast String, Type.getClass("foo"));
	}

	function testGetSuperClass() {
		eq(cast java.lang.Object, Type.getSuperClass(String));
	}

	function testGetClassName() {
		eq("test._TestTypeApi.SomeClass", Type.getClassName(SomeClass));
		eq("java.lang.String", Type.getClassName(String));
	}

	function testResolveClass() {
		eq(cast String, Type.resolveClass("java.lang.String"));
		// eq(cast String, Type.resolveClass("String")); // TODO
		eq(cast SomeClass, Type.resolveClass("test._TestTypeApi.SomeClass"));
		eq(null, Type.resolveClass("i.dont.Exist"));
		eq(null, Type.resolveClass("in valid %%#'#! String"));
	}
}
