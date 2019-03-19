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
}