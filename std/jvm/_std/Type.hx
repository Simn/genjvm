import java.lang.invoke.*;
import java.lang.NoSuchMethodException;
import java.jvm.annotation.EnumReflectionInformation;

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
		var c = (cast o : java.lang.Object).getClass();
		// TODO: have to be careful if we ever decide to omit EnumReflectionInformation
		// Maybe a separate HaxeEnum annotation would be better here
		if (c.isAnnotationPresent(cast EnumReflectionInformation)) {
			return null;
		}
		return c;
	}

	public static function getEnum(o:EnumValue):Enum<Dynamic> {
		if (o == null) {
			return null;
		}
		var c = (cast o : java.lang.Object).getClass();
		if (!c.isAnnotationPresent(cast EnumReflectionInformation)) {
			return null;
		}
		return cast c;
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
		var argTypes:java.NativeArray<java.lang.Class<Dynamic>> = new java.NativeArray(args.length);
		var cl = cl.native();
		for (i in 0...args.length) {
			var arg = (cast args[i] : java.lang.Object);
			argTypes[i] = arg.getClass();
			args[i] = arg;
		}
		var methodType = MethodType.methodType(cast Void, argTypes);

		var ctor2 = try {
			MethodHandles.lookup().findConstructor(cl, methodType);
		} catch (_:NoSuchMethodException) {
			null;
		}
		if (ctor2 == null) {
			for (ctor in cl.getConstructors()) {
				var params = ctor.getParameterTypes();
				if (params.length != args.length) {
					continue;
				}
				var valid = true;
				for (i in 0...params.length) {
					if (!java.jvm.Jvm.getWrapperClass(params[i]).isAssignableFrom(argTypes[i])) {
						valid = false;
						break;
					}
				}
				if (valid) {
					ctor2 = MethodHandles.lookup().unreflectConstructor(ctor);
					break;
				}
			}
		}
		if (ctor2 != null) {
			return ctor2.invokeWithArguments(@:privateAccess args.__a);
		}
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
		var clInfo:Class<Dynamic> = cast EnumReflectionInformation;
		// TODO: We should use e.getAnnotation(clInfo) here, but our type system has some issues with that
		var annotations = e.native().getAnnotations();
		for (annotation in annotations) {
			if (annotation.annotationType() == clInfo) {
				return @:privateAccess Array.ofNative((cast annotation : EnumReflectionInformation).constructorNames());
			}
		}
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
