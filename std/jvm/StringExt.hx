package jvm;

import java.lang.JavaString.String as JavaString;

class StringExt {
	public static function charAt(me:String, index:Int):String {
		if (index >= me.length || index < 0)
			return "";
		else
			return java.lang.Character._toString((cast me : JavaString).charAt(index));
	}

	public static function charCodeAt(me:String, index:Int):Null<Int> {
		if (index >= me.length || index < 0)
			return null;
		else
			return cast((cast me : JavaString).charAt(index), Int);
	}
}
