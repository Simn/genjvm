package test;


class NativeExceptionBase extends java.lang.Exception {
	public function new(message:String) {
		super(message);
	}
}

class NativeExceptionChild extends NativeExceptionBase {
	public function new(message:String) {
		super(message);
	}
}

class NativeExceptionOther extends java.lang.Exception {
	public function new(message:String) {
		super(message);
	}
}

class TestExceptions extends BaseTest {
	public function new() {
		super();
		test();
		testNested();
	}

	function test() {
		eq("caught NativeExceptionChild: msg", raise(() -> throw new NativeExceptionChild("msg")));
		eq("caught NativeExceptionBase: msg", raise(() -> throw new NativeExceptionBase("msg")));
		eq("caught String: msg", raise(() -> throw "msg"));
		eq("caught NativeExceptionOther: msg", raise(() -> throw new NativeExceptionOther("msg")));
		eq("caught Int: 12", raise(() -> throw 12));
		eq("caught Dynamic: 12.0", raise(() -> throw 12.));
		eq("caught Dynamic: false", raise(() -> throw false));
		eq("caught Throwable: msg", raise(() -> throw new java.lang.Exception("msg")));
	}

	function testNested() {
		var s = try {
			try {
				throw "foo";
			} catch(e:Int) {
				"something went wrong";
			}
		} catch(e:String) {
			e;
		}
		eq("foo", s);
	}

	static function raise<T>(f:Void -> String) {
		return try {
			f();
		} catch(e:NativeExceptionChild) {
			'caught NativeExceptionChild: ${e.getMessage()}';
		} catch(e:NativeExceptionBase) {
			'caught NativeExceptionBase: ${e.getMessage()}';
		} catch(e:String) {
			'caught String: $e';
		} catch(e:NativeExceptionOther) {
			'caught NativeExceptionOther: ${e.getMessage()}';
		} catch(e:Int) {
			'caught Int: $e';
 		} catch(e:java.lang.Throwable) {
			'caught Throwable: ${e.getMessage()}';
		} catch(e:Dynamic) {
			'caught Dynamic: $e';
		}
	}
}