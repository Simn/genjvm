import java.lang.invoke.*;
import java.lang.NoSuchMethodException;
import java.jvm.annotation.ClassReflectionInformation;
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
	static function hackGetAnnotation<T:java.lang.annotation.Annotation>(c:java.lang.Class<Dynamic>, cAnnotation:java.lang.Class<T>):T {
		// TODO: We should use e.getAnnotation(clInfo) here, but our type system has some issues with that
		var annotations = c.getAnnotations();
		for (annotation in annotations) {
			if (cAnnotation == cast annotation.annotationType()) {
				return cast annotation;
			}
		}
		return null;
	}

	static function isEnum<T>(c:java.lang.Class<T>):Bool {
		// TODO: have to be careful if we ever decide to omit EnumReflectionInformation
		// Maybe a separate HaxeEnum annotation would be better here
		return c.isAnnotationPresent(cast EnumReflectionInformation);
	}

	public static function getClass<T>(o:T):Class<T> {
		if (o == null) {
			return null;
		}
		if (java.jvm.Jvm.instanceof(o, Class)) {
			return null;
		}
		var c = (cast o : java.lang.Object).getClass();
		if (isEnum(c)) {
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
		var c = c.native();
		var cSuper = c.getSuperclass();
		if (cSuper == null) {
			return null;
		}
		var annotation = hackGetAnnotation(c, (cast ClassReflectionInformation : java.lang.Class<ClassReflectionInformation>));
		if (annotation != null && annotation.hasSuperClass() == false) {
			return null;
		}
		return cSuper;
	}

	public static function getClassName(c:Class<Dynamic>):String {
		// TODO: java.lang.String has to become String somehow
		return c.native().getName();
	}

	public static function getEnumName(e:Enum<Dynamic>):String {
		return e.native().getName();
	}

	public static function resolveClass(name:String):Class<Dynamic> {
		return try {
			java.lang.Class.forName(name);
		} catch (e:java.lang.ClassNotFoundException) {
			return null;
		}
	}

	public static function resolveEnum(name:String):Enum<Dynamic> {
		return try {
			var c = java.lang.Class.forName(name);
			if (!isEnum(c)) {
				null;
			} else {
				cast c;
			}
		} catch (e:java.lang.ClassNotFoundException) {
			return null;
		}
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
		return cl.native().newInstance();
	}

	public static function createEnum<T>(e:Enum<T>, constr:String, ?params:Array<Dynamic>):T {
		return null;
	}

	public static function createEnumIndex<T>(e:Enum<T>, index:Int, ?params:Array<Dynamic>):T {
		// TODO: review this if we ever do nadako-enums
		return cast new java.jvm.Enum(index, params == null ? java.NativeArray.make(0) : @:privateAccess params.__a);
	}

	public static function getInstanceFields(c:Class<Dynamic>):Array<String> {
		return null;
	}

	public static function getClassFields(c:Class<Dynamic>):Array<String> {
		return null;
	}

	public static function getEnumConstructs(e:Enum<Dynamic>):Array<String> {
		var clInfo:java.lang.Class<EnumReflectionInformation> = cast EnumReflectionInformation;
		var annotation = hackGetAnnotation(e.native(), clInfo);
		return @:privateAccess Array.ofNative(annotation.constructorNames());
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
		return @:privateAccess Array.ofNative((cast e : java.jvm.Enum).parameters);
	}

	public static function enumIndex(e:EnumValue):Int {
		return (cast e : java.jvm.Enum).index;
	}

	public static function allEnums<T>(e:Enum<T>):Array<T> {
		return null;
	}
}
