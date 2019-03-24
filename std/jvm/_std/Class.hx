@:coreType
@:runtimeValue
@:native("java.lang.Class")
abstract Class<T> from java.lang.Class<Dynamic> {
	public inline function native():java.lang.Class<Dynamic>
		return cast this;
}
