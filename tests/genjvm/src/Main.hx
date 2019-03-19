import haxe.PosInfos;
import java.lang.System;

@:analyzer(ignore)
class Main {
	static public function main() {
		new Main();
	}

	var numTests:Int;
	var numFailures:Int;
	var intField:Int;

	static var staticIntField:Int;

	function new() {
		numTests = 0;
		numFailures = 0;
		testAssignment();
		System.out.print("Done! ");
		System.out.print(numTests);
		System.out.print(" tests with ");
		System.out.print(numFailures);
		System.out.println(" failures");
	}

	function testAssignment() {
		// var
		var a = 1;
		eq(1, a);
		a = 2;
		eq(2, a);
		a += 1;
		eq(3, a);
		eq(3, a++);
		eq(4, a);
		eq(5, ++a);

		// instance field
		intField = 1;
		eq(1, intField);
		intField = 2;
		eq(2, intField);
		intField += 1;
		eq(3, intField);
		eq(3, intField++);
		eq(4, intField);
		eq(5, ++intField);

		// instance field
		staticIntField = 1;
		eq(1, staticIntField);
		staticIntField = 2;
		eq(2, staticIntField);
		staticIntField += 1;
		eq(3, staticIntField);
		eq(3, staticIntField++);
		eq(4, staticIntField);
		eq(5, ++staticIntField);
	}

	// has to be inline at the moment because TObjectDecl for PosInfos doesn't work yet
	inline function eq<T>(expected:T, actual:T, ?p:PosInfos) {
		++numTests;
		if (expected != actual) {
			++numFailures;
			System.out.print(p.fileName);
			System.out.print(":");
			System.out.print(p.lineNumber);
			System.out.print(": ");
			System.out.print(actual);
			System.out.print(" should be ");
			System.out.println(expected);
		}
	}
}
