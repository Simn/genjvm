package jvm;

@:keep
@:native('haxe.jvm.Object')
@:nativeGen
class Object {
	public function new() { }

	public function _hx_getField(name:String) {
		return Jvm.readFieldNoObject(this, name);
	}
}
