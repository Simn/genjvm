enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass(c:Class<Dynamic>);
	TEnum(e:Enum<Dynamic>);
	TUnknown;
}

@:coreApi
class Type {
	public static function getClass<T>(o:T):Class<T> {
		if (o == null) {
			return null;
		}
		if (java.jvm.Jvm.instanceof(o, Class)) {
			return null;
		}
		return (cast o : java.lang.Object).getClass();
	}

	public static function getEnum(o:EnumValue):Enum<Dynamic> {
		return null;
	}

	public static function getSuperClass(c:Class<Dynamic>):Class<Dynamic> {
		// TODO: we have to not report haxe.lang.Object for Haxe base types
		return c.native().getSuperclass();
	}

	public static function getClassName(c:Class<Dynamic>):String {
		// TODO: java.lang.String has to become String somehow
		return c.native().getName();
	}

	public static function getEnumName(e:Enum<Dynamic>):String {
		return null;
	}

	public static function resolveClass(name:String):Class<Dynamic> {
		return try {
			java.lang.Class.forName(name);
		} catch (e:java.lang.ClassNotFoundException) {
		return null;
	}
	}

	public static function resolveEnum(name:String):Enum<Dynamic> {
		return null;
	}

	public static function createInstance<T>(cl:Class<T>, args:Array<Dynamic>):T {
		return null;
	}

	public static function createEmptyInstance<T>(cl:Class<T>):T {
		return null;
	}

	public static function createEnum<T>(e:Enum<T>, constr:String, ?params:Array<Dynamic>):T {
		return null;
	}

	public static function createEnumIndex<T>(e:Enum<T>, index:Int, ?params:Array<Dynamic>):T {
		return null;
	}

	public static function getInstanceFields(c:Class<Dynamic>):Array<String> {
		return null;
	}

	public static function getClassFields(c:Class<Dynamic>):Array<String> {
		return null;
	}

	public static function getEnumConstructs(e:Enum<Dynamic>):Array<String> {
		return null;
	}

	public static function typeof(v:Dynamic):ValueType {
		return null;
	}

	public static function enumEq<T>(a:T, b:T):Bool {
		return false;
	}

	public static function enumConstructor(e:EnumValue):String {
		return null;
	}

	public static function enumParameters(e:EnumValue):Array<Dynamic> {
		return null;
	}

	public static function enumIndex(e:EnumValue):Int {
		return 0;
	}

	public static function allEnums<T>(e:Enum<T>):Array<T> {
		return null;
	}
}
