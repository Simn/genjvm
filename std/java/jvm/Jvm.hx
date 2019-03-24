package java.jvm;

import java.jvm.Enum;
import java.jvm.DynamicObject;
import java.jvm.Exception;

@:keep
@:native('haxe.jvm.Jvm')
class Jvm {
	extern static public function instanceof<S, T>(obj:S, type:T):Bool;

	extern static public function referenceEquals<T>(v1:T, v2:T):Bool;

	static public function equals<T>(v1:T, v2:T):Bool {
		if (referenceEquals(v1, v2))
			return true;
		if (v1 == null || v2 == null)
			return false;

		if (instanceof(v1, java.lang.Number)) {
			if (!(instanceof(v2, java.lang.Number)))
				return false;

			var v1c = (cast v1 : java.lang.Number);
			var v2c = (cast v2 : java.lang.Number);
			if (instanceof(v1, java.lang.Long) || instanceof(v2, java.lang.Long)) {
				return v1c.longValue() == v2c.longValue();
			}
			return v1c.doubleValue() == v2c.doubleValue();
		} else if (instanceof(v1, java.lang.JavaString.String)) {
			return (cast v1 : java.lang.JavaString.String).equals(v2);
		}

		return false;
	}

	static public function getMethodHandle(on:java.lang.Class<Dynamic>, name:String, rtype:java.lang.Class<Dynamic>,
			types:java.NativeArray<java.lang.Class<Dynamic>>) {
		var lut = java.lang.invoke.MethodHandles.lookup();
		var mt = java.lang.invoke.MethodType.methodType(rtype, types);
		return lut.findStatic(on, name, mt);
	}

	static public function toInt(d:Dynamic) {
		return d == null ? 0 : (d : java.lang.Number).intValue();
	}

	static public function toDouble(d:Dynamic) {
		return d == null ? 0. : (d : java.lang.Number).doubleValue();
	}

	static public function toFloat(d:Dynamic):Single {
		return d == null ? 0. : (d : java.lang.Number).floatValue();
	}

	static public function toBoolean(d:Dynamic) {
		return d == null ? false : (d : java.lang.Boolean).booleanValue();
	}

	static public function bindMethod<T:java.lang.Object>(obj:T, name:String, descriptor:String) {
		return java.lang.invoke.MethodHandles.lookup()
			.findVirtual(obj.getClass(), name,
				java.lang.invoke.MethodType.fromMethodDescriptorString(descriptor, java.lang.ClassLoader.getSystemClassLoader())).bindTo(obj);
	}

	static public function readField(obj:Dynamic, name:String):Dynamic {
		if (obj == null) {
			return null;
		}
		var isStatic = instanceof(obj, java.lang.Class);
		var cl = isStatic ? obj : (obj : java.lang.Object).getClass();
		try {
			var field = cl.getField(name);
			field.setAccessible(true);
			return field.get(obj);
		} catch (_:java.lang.NoSuchFieldException) {
			while (cl != null) {
				var methods = cl.getMethods();
				for (m in methods) {
					if (m.getName() == name) {
						var method = java.lang.invoke.MethodHandles.lookup().unreflect(m);
						if (!isStatic || cl == cast java.lang.Class) {
							method = method.bindTo(obj);
						}
						return method;
					}
				}
				if (instanceof(obj, java.jvm.DynamicObject)) {
					return (obj : java.jvm.DynamicObject)._hx_getField(name);
				}
				if (isStatic) {
					if (cl == cast java.lang.Class) {
						break;
					}
					cl = cast java.lang.Class;
				} else {
					cl = cl.getSuperclass();
				}
			}
			return null;
		}
	}

	static public function writeField<T>(obj:Dynamic, name:String, value:T) {
		if (obj == null) {
			return;
		}
		try {
			var cl = (obj : java.lang.Object).getClass();
			var field = cl.getField(name);
			field.setAccessible(true);
			field.set(obj, value);
		} catch (_:java.lang.NoSuchFieldException) {
			if (instanceof(obj, java.jvm.DynamicObject)) {
				return (obj : java.jvm.DynamicObject)._hx_setField(name, value);
			}
			return;
		}
	}

	static public function toString<T:java.lang.Object>(obj:T) {
		if (obj == null) {
			return "null";
		} else {
			return obj.toString();
		}
	}

	static public function stringConcat<A:java.lang.Object, B:java.lang.Object>(a:A, b:B):String {
		return (cast toString(a) : java.lang.JavaString.String).concat(toString(b));
	}

	static public function getWrapperClass<S, T>(c:java.lang.Class<S>):java.lang.Class<S> {
		if (!c.isPrimitive()) {
			return c;
		}
		// TODO: other basic types
		return if (c == cast Int) {
			cast java.lang.Integer.IntegerClass;
		} else if (c == cast Float) {
			cast java.lang.Double.DoubleClass;
		} else if (c == cast Bool) {
			cast java.lang.Boolean.BooleanClass;
		} else {
			c;
		}
	}
}
