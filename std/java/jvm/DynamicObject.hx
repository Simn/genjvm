package java.jvm;

import java.NativeArray;

@:native("haxe.jvm.DynamicObject")
class DynamicObject {
	var values:NativeArray<Dynamic>;
	var lookup:Map<String, Int>;

	public function new(values:NativeArray<Dynamic>, lookup:Map<String, Int>) {
		this.values = values;
		this.lookup = lookup;
	}

	@:overload public function readField(index:Int) {
		return values[index];
	}

	@:overload public function readField(name:String) {
		return values[lookup[name]];
	}

	@:overload public function writeField(index:Int, value:Dynamic) {
		values[index] = value;
	}

	@:overload public function writeField(name:String, value:Dynamic) {
		values[lookup[name]] = value;
	}
}