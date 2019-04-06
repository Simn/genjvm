package jvm;

import java.NativeArray;

@:keep
@:native('haxe.jvm.Enum')
class Enum {
	public var _hx_index:Int;

	@:nativeGen public function new(index:Int) {
		this._hx_index = index;
	}

	public function toString() {
		var baseName = Type.getEnumConstructs(Type.getEnum(cast this))[_hx_index];
		var parameters = Type.enumParameters(cast this);
		if (parameters.length == 0) {
			return baseName;
		}
		return '$baseName(${@:privateAccess parameters.join(",")})';
	}
}
