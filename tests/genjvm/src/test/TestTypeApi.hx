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

private class SomeClassWithUnwrappedArgs {
	public var value:String;

	@:overload public function new(i:Int, s:String) {
		value = i + s;
	}

	@:overload public function new(b:Bool, s:String) {
		value = b + s;
	}
}

private class SomeClassWithWrappedArgs {
	public var value:String;

	@:overload public function new(i:Null<Int>, s:String) {
		value = i + s;
	}

	@:overload public function new(i:Null<Bool>, s:String) {
		value = i + s;
	}
}

private class SomeClassWithMixedArgs {
	public var value:String;

	@:overload public function new(i:Null<Int>, i2:Int) {
		value = "" + i + i2;
	}

	@:overload public function new(i:Null<Bool>, i2:Null<Bool>) {
		value = "" + i + i2;
	}
}

class TestTypeApi extends BaseTest {
	public function new() {
		super();
		testGetClass();
		testGetSuperClass();
		testGetClassName();
		testResolveClass();
		testCreateInstance();
		testEnumConstructs();
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

	function testCreateInstance() {
		function wrap<T>(i:T):Null<T> {
			return i;
		}
		eq("12foo", Type.createInstance(SomeClassWithUnwrappedArgs, [12, "foo"]).value);
		eq("12foo", Type.createInstance(SomeClassWithUnwrappedArgs, [wrap(12), "foo"]).value);
		eq("truefoo", Type.createInstance(SomeClassWithUnwrappedArgs, [true, "foo"]).value);
		eq("truefoo", Type.createInstance(SomeClassWithUnwrappedArgs, [wrap(true), "foo"]).value);

		eq("12foo", Type.createInstance(SomeClassWithWrappedArgs, [12, "foo"]).value);
		eq("12foo", Type.createInstance(SomeClassWithWrappedArgs, [wrap(12), "foo"]).value);
		eq("truefoo", Type.createInstance(SomeClassWithWrappedArgs, [true, "foo"]).value);
		eq("truefoo", Type.createInstance(SomeClassWithWrappedArgs, [wrap(true), "foo"]).value);

		eq("1212", Type.createInstance(SomeClassWithMixedArgs, [12, 12]).value);
		eq("1212", Type.createInstance(SomeClassWithMixedArgs, [wrap(12), 12]).value);
		eq("1212", Type.createInstance(SomeClassWithMixedArgs, [12, wrap(12)]).value);
		eq("1212", Type.createInstance(SomeClassWithMixedArgs, [wrap(12), wrap(12)]).value);

		eq("truetrue", Type.createInstance(SomeClassWithMixedArgs, [true, true]).value);
		eq("truetrue", Type.createInstance(SomeClassWithMixedArgs, [wrap(true), true]).value);
		eq("truetrue", Type.createInstance(SomeClassWithMixedArgs, [true, wrap(true)]).value);
		eq("truetrue", Type.createInstance(SomeClassWithMixedArgs, [wrap(true), wrap(true)]).value);
	}

	function testEnumConstructs() {
		var a = Type.getEnumConstructs(haxe.ds.Option);
		eq("Some", a[0]);
		eq("None", a[1]);
	}
}
