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

	public static function split(me:String, delimiter:String):Array<String> {
		var ret = [];
		if (delimiter.length == 0) {
			for (i in 0...me.length) {
				ret.push(me.charAt(i));
			}
		} else {
			var start = 0;
			var pos = me.indexOf(delimiter, start);
			while (pos >= 0) {
				ret.push((cast me : java.lang.JavaString.String).substring(start, pos));
				start = pos + delimiter.length;
				pos = me.indexOf(delimiter, start);
			}
			ret.push((cast me : java.lang.JavaString.String).substring(start));
		}
		return ret;
	}

	public static function substring(me:String, startIndex:Int, ?endIndex:Int):String {
		var endIndex:Int = endIndex == null ? me.length : endIndex;
		if (endIndex < 0) {
			endIndex = 0;
		} else if (endIndex > me.length) {
			endIndex = me.length;
		}
		if (startIndex < 0) {
			startIndex = 0;
		} else if (startIndex > me.length) {
			startIndex = me.length;
		}

		if (startIndex > endIndex) {
			var tmp = startIndex;
			startIndex = endIndex;
			endIndex = tmp;
		}
		return (cast me : java.lang.JavaString.String).substring(startIndex, endIndex);
	}
}
