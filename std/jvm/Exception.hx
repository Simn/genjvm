package jvm;

@:keep
@:native('haxe.jvm.Exception')
class Exception<T> extends java.lang.Exception {
	public var value:T;

	public function new(value:T) {
		super();
		this.value = value;
	}

	@:overload override public function toString() {
		return Std.string(value);
	}

	public function unwrap() {
		return value;
	}

	static public function wrap<T>(t:Null<T>) {
		if (Jvm.instanceof(t, java.lang.Exception)) {
			return (cast t : java.lang.Exception);
		} else {
			return new Exception(t);
		}
	}
}
