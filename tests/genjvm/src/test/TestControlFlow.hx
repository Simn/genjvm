package test;

@:keep
@:analyzer(ignore)
class TestControlFlow {
	static function whileTrue():Int {
		while (true) {
			Sys.println("don't call this function");
		}
	}

	static function whileTrueReturn():Int {
		while (true) {
			return 1;
		}
	}

	static function whileTrueThrow():Int {
		while (true) {
			throw "oops";
		}
	}
}
