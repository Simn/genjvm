package jvm;

extern class NativeClassTools {
	static public inline function native<T>(c:Class<T>):java.lang.Class<T> {
		return cast c;
	}

	static public inline function haxe<T>(c:java.lang.Class<T>):Class<T> {
		return cast c;
	}
}

extern class NativeEnumTools {
	static public inline function native<T>(e:std.Enum<Dynamic>):java.lang.Class<T> {
		return cast e;
	}
}