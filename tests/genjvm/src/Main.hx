class Main {
	static public function main() {
		new test.TestChaos();
		new test.TestOop();
		new test.TestMatch();
		new test.TestExceptions();
		new test.TestClosure();
		new test.TestTypeApi();
		trace('Done! ${test.BaseTest.numTests} tests with ${test.BaseTest.numFailures} failures');
	}
}
