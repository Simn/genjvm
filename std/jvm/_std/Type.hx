import java.lang.invoke.*;
import java.lang.NoSuchMethodException;
import jvm.annotation.*;
import jvm.Jvm;

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

	static function isEnumClass<T>(c:java.lang.Class<T>):Bool {
		// TODO: have to be careful if we ever decide to omit EnumReflectionInformation
		// Maybe a separate HaxeEnum annotation would be better here
		return c.isAnnotationPresent(cast EnumReflectionInformation);
	}

	static function isEnumValueClass<T>(c:java.lang.Class<T>):Bool {
		// TODO: have to be careful if we ever decide to omit EnumValueReflectionInformation
		// Maybe a separate HaxeEnum annotation would be better here
		return c.isAnnotationPresent(cast EnumValueReflectionInformation);
	}

	public static function getClass<T>(o:T):Class<T> {
		if (o == null) {
			return null;
		}
		if (Jvm.instanceof(o, Class)) {
			return null;
		}
		var c = (cast o : java.lang.Object).getClass();
		if (isEnumValueClass(c)) {
			return null;
		}
		return c;
	}

	public static function getEnum(o:EnumValue):Enum<Dynamic> {
		if (o == null) {
			return null;
		}
		var c = (cast o : java.lang.Object).getClass().getSuperclass();
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
			if (!isEnumClass(c)) {
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
					if (!Jvm.getWrapperClass(params[i]).isAssignableFrom(argTypes[i])) {
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
		if (params == null || params.length == 0) {
			return Jvm.readField(e, constr);
		} else {
			return Reflect.callMethod(null, Jvm.readField(e, constr), params);
		}
	}

	public static function createEnumIndex<T>(e:Enum<T>, index:Int, ?params:Array<Dynamic>):T {
		var clInfo:java.lang.Class<EnumReflectionInformation> = cast EnumReflectionInformation;
		var annotation = hackGetAnnotation(e.native(), clInfo);
		if (params == null || params.length == 0) {
			return Jvm.readField(e, annotation.constructorNames()[index]);
		} else {
			return Reflect.callMethod(null, Jvm.readField(e, annotation.constructorNames()[index]), params);
		}
	}

	static function getFields<T>(c:java.lang.Class<T>, statics:Bool):Array<String> {
		var ret = [];
		for (f in c.getDeclaredFields()) {
			if (java.lang.reflect.Modifier.isStatic(f.getModifiers()) == statics) {
				ret.push(f.getName());
			}
		}
		for (m in c.getDeclaredMethods()) {
			if (java.lang.reflect.Modifier.isStatic(m.getModifiers()) == statics) {
				ret.push(m.getName());
			}
		}
		return ret;
	}

	public static function getInstanceFields(c:Class<Dynamic>):Array<String> {
		return getFields(c.native(), false);
	}

	public static function getClassFields(c:Class<Dynamic>):Array<String> {
		return getFields(c.native(), true);
	}

	public static function getEnumConstructs(e:Enum<Dynamic>):Array<String> {
		var clInfo:java.lang.Class<EnumReflectionInformation> = cast EnumReflectionInformation;
		var annotation = hackGetAnnotation(e.native(), clInfo);
		return @:privateAccess Array.ofNative(annotation.constructorNames());
	}

	public static function typeof(v:Dynamic):ValueType {
		// could optimize this with an annotation on Haxe classes
		if (v == null) {
			return TNull;
		}
		if (Jvm.instanceof(v, java.lang.Number)) {
			var v:java.lang.Number = cast v;
			if (v.intValue() == v.doubleValue()) {
				return TInt;
			}
			return TFloat;
		}
		if (Jvm.instanceof(v, java.lang.Boolean.BooleanClass)) {
			return TBool;
		}
		if (Jvm.instanceof(v, jvm.DynamicObject)) {
			return TObject;
		}
		if (Jvm.instanceof(v, java.lang.invoke.MethodHandle)) {
			return TFunction;
		}
		var c = (cast v : java.lang.Object).getClass();
		// TODO: native enums?
		if (isEnumValueClass(c)) {
			return TEnum(cast c.getSuperclass());
		}
		return TClass(c);
	}

	public static function enumEq<T>(a:T, b:T):Bool {
		var a:jvm.Enum = cast a;
		var b:jvm.Enum = cast b;
		if (a._hx_index != b._hx_index) {
			return false;
		}
		var params1 = enumParameters(cast a);
		var params2 = enumParameters(cast b);
		if (params1.length != params2.length) {
			return false;
		}
		for (i in 0...params1.length) {
			if (params1[i] != params2[i]) {
				return false;
			}
		}
		return true;
	}

	public static function enumConstructor(e:EnumValue):String {
		var clInfo:java.lang.Class<EnumReflectionInformation> = cast EnumReflectionInformation;
		var annotation = hackGetAnnotation(getEnum(e).native(), clInfo);
		if (annotation == null) {
			return null;
		}
		return annotation.constructorNames()[(cast e : jvm.Enum)._hx_index];
	}

	public static function enumParameters(e:EnumValue):Array<Dynamic> {
		var clInfo:java.lang.Class<EnumValueReflectionInformation> = cast EnumValueReflectionInformation;
		var annotation = hackGetAnnotation((cast e : java.lang.Object).getClass(), clInfo);
		var ret = [];
		if (annotation == null) {
			return ret;
		}
		for (name in annotation.argumentNames()) {
			ret.push(Jvm.readField(e, name));
		}
		return ret;
	}

	public static function enumIndex(e:EnumValue):Int {
		return (cast e : jvm.Enum)._hx_index;
	}

	public static function allEnums<T>(e:Enum<T>):Array<T> {
		var all = getEnumConstructs(e);
		var ret = [];
		for (name in all) {
			var v = Jvm.readField(e, name);
			if (Jvm.instanceof(v, jvm.Enum)) {
				ret.push(v);
			}
		}
		return ret;
	}
}
