package java.jvm.annotation;

@:annotation
@:native("haxe.jvm.annotation.EnumReflectionInformation")
@:keep
interface EnumReflectionInformation {
	function constructorNames():java.NativeArray<String>;
}
