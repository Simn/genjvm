package test;

import haxe.ds.Option;
import Type.ValueType;

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

private class ActuallyExtendsObject extends java.lang.Object {}

class TestTypeApi extends BaseTest {
	public function new() {
		super();
		testGetClass();
		testGetEnum();
		testGetSuperClass();
		testGetClassName();
		testGetEnumName();
		testResolveClass();
		testResolveEnum();
		testCreateInstance();
		testCreateEmptyInstance();
		testEnumConstructs();
		testTypeof();
		testEnumIndex();
		testEnumParameters();
	}

	function testGetClass() {
		eq(null, Type.getClass(null));
		eq(null, Type.getClass(TestTypeApi));
		eq(null, Type.getClass(java.lang.Object));
		eq(cast SomeClass, Type.getClass(new SomeClass()));
		eq(cast String, Type.getClass("foo"));
		eq(null, Type.getClass(None));
	}

	function testGetEnum() {
		eq(cast Option, Type.getEnum(None));
	}

	function testGetSuperClass() {
		eq(cast java.lang.Object, Type.getSuperClass(String));
		eq(cast null, Type.getSuperClass(SomeClass));
		eq(cast java.lang.Object, Type.getSuperClass(ActuallyExtendsObject));
	}

	function testGetClassName() {
		eq("test._TestTypeApi.SomeClass", Type.getClassName(SomeClass));
		eq("java.lang.String", Type.getClassName(String));
	}

	function testGetEnumName() {
		eq("haxe.macro.ExprDef", Type.getEnumName(haxe.macro.Expr.ExprDef));
	}

	function testResolveClass() {
		eq(cast String, Type.resolveClass("java.lang.String"));
		// eq(cast String, Type.resolveClass("String")); // TODO
		eq(cast SomeClass, Type.resolveClass("test._TestTypeApi.SomeClass"));
		eq(null, Type.resolveClass("i.dont.Exist"));
		eq(null, Type.resolveClass("in valid %%#'#! String"));
	}

	function testResolveEnum() {
		eq(null, Type.resolveEnum("java.lang.String"));
		eq(null, Type.resolveEnum("String")); // TODO
		eq(null, Type.resolveEnum("test._TestTypeApi.SomeClass"));
		eq(null, Type.resolveEnum("i.dont.Exist"));
		eq(null, Type.resolveEnum("in valid %%#'#! String"));
		eq(cast Option, Type.resolveEnum("haxe.ds.Option"));
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

	function testCreateEmptyInstance() {
		eq(cast SomeClass, Type.getClass(Type.createEmptyInstance(SomeClass)));
		// TODO: ouch...
		// eq(cast SomeClassWithArgs, Type.getClass(Type.createEmptyInstance(SomeClassWithArgs)));
	}

	function testEnumConstructs() {
		var a = Type.getEnumConstructs(Option);
		eq("Some", a[0]);
		eq("None", a[1]);
	}

	function testTypeof() {
		// enum comparison is broken
		// eq(TNull, Type.typeof(null));
		// eq(TInt, Type.typeof(0));
		// eq(TInt, Type.typeof(0.)); // is this right?
		// eq(TFloat, Type.typeof(0.1));
		// // eq(Type.typeof(false)); // TODO
		// // eq(Type.typeof(true));
		// eq(TFunction, Type.typeof(testTypeof));
		// eq(TFunction, Type.typeof(function() {}));
		// // eq(Type.typeof(haxe.ds.Option.None));
		// // eq(Type.typeof(haxe.ds.Option.Some(1)));
		// eq(TObject, Type.typeof({}));
		// eq(Type.typeof("foo"));
	}

	function testEnumIndex() {
		eq(1, Type.enumIndex(None));
		eq(0, Type.enumIndex(Some(12)));
	}

	function testEnumParameters() {
		eq(0, Type.enumParameters(None).length);
		eq(12, Type.enumParameters(Some(12))[0]);
	}
}
