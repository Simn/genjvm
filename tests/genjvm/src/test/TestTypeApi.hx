package test;

private class SomeClass {
	public function new() {}
}

class TestTypeApi extends BaseTest {
	public function new() {
		super();
		testGetClass();
		testGetSuperClass();
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
}
