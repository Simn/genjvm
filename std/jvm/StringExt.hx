package jvm;

import java.lang.JavaString.String as JavaString;

class StringExt {
	public static function charAt(me:String, index:Int):String {
		if (index >= me.length || index < 0)
			return "";
		else
			return java.lang.Character._toString((cast me : JavaString).charAt(index));
	}
}
