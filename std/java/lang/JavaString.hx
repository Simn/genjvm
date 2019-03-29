package java.lang;

extern class String {
	public function charAt(index:Int):java.types.Char16;
	public function compareTo(s:std.String):Int;
	public function concat(s:std.String):std.String;
	public function equals(obj:Dynamic):Bool;
	public function hashCode():Int;
}