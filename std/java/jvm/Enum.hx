package java.jvm;

@:keep
@:native('haxe.jvm.Enum')
class Enum {
	public var index:Int;
	public var parameters:NativeArray<Dynamic>;

	public function new(index:Int, parameters:NativeArray<Dynamic>) {
		this.index = index;
		this.parameters = parameters;
	}

	public function toString() {
		var baseName = Type.getEnumConstructs(cast(cast this : java.lang.Object).getClass())[index];
		if (parameters.length == 0) {
			return baseName;
		}
		// TODO: creating arrays here is stupid
		return '$baseName(${@:privateAccess Array.ofNative(parameters).join(", ")})';
	}
}
