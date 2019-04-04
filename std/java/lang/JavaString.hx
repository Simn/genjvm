package java.lang;

extern class String {
	function charAt(index:Int):java.types.Char16;
	function codePointAt(index:Int):Int;
	function compareTo(s:std.String):Int;
	function concat(s:std.String):std.String;
	function endsWith(suffix:std.String):Bool;
	function equals(obj:Dynamic):Bool;
	function hashCode():Int;

	@:overload function indexOf(str:std.String):Int;
	@:overload function indexOf(str:std.String, fromIndex:Int):Int;

	function replace(target:CharSequence, replacement:CharSequence):std.String;
	function startsWith(prefix:std.String):Bool;
}
