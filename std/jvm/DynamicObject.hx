package jvm;

import haxe.ds.StringMap;

@:keep
@:native('haxe.jvm.DynamicObject')
class DynamicObject implements java.lang.Cloneable {
	var _hx_fields:Null<StringMap<Dynamic>>;

	public var _hx_deletedAField:Null<Int>;

	@:nativeGen public function new() {}

	public function toString() {
		_hx_initReflection();
		var buf = new StringBuf();
		buf.addChar("{".code);
		var first = true;
		for (key in _hx_fields.keys()) {
			buf.add(key);
			buf.add(": ");
			buf.add(_hx_fields.get(key));
			if (first) {
				first = false;
				buf.add(", ");
			}
		}
		buf.addChar("}".code);
		return buf.toString();
	}

	final public function _hx_deleteField(name:String) {
		_hx_initReflection();
		_hx_deletedAField = 1;
		try {
			Jvm.writeFieldNoDyn(this, name, null);
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
		try {
			Jvm.writeFieldNoDyn(this, name, value);
		} catch (_:Dynamic) {}
	}

	final public function _hx_clone() {
		var clone:DynamicObject = (cast this : java.lang.Object).clone();
		if (_hx_fields != null) {
			clone._hx_fields = this._hx_fields.copy();
		}
		return clone;
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
