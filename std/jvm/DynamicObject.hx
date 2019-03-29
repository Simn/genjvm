package jvm;

import haxe.ds.StringMap;

@:keep
@:native('haxe.jvm.DynamicObject')
class DynamicObject {
	var _hx_fields:Null<StringMap<Dynamic>>;
	public var _hx_deletedAField:Null<Int>;

	public function new() {}

	final public function _hx_deleteField(name:String) {
		_hx_initReflection();
		_hx_deletedAField = 1;
		try {
			Reflect.setField(this, name, null);
		} catch (_:Dynamic) {}
		return _hx_fields.remove(name);
	}

	final public function _hx_getFields() {
		_hx_initReflection();
		return [for (key in _hx_fields.keys()) key];
	}

	final public function _hx_getField<T>(name:String) {
		_hx_initReflection();
		return _hx_fields.get(name);
	}

	final public function _hx_hasField(name:String) {
		_hx_initReflection();
		return _hx_fields.exists(name);
	}

	final public function _hx_setField<T>(name:String, value:T) {
		_hx_initReflection();
		_hx_fields.set(name, value);
	}

	final function _hx_initReflection() {
		if (_hx_fields == null) {
			_hx_fields = _hx_getKnownFields();
		}
	}

	function _hx_getKnownFields():StringMap<Dynamic> {
		return new StringMap();
	}
}
