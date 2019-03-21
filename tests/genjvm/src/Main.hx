class Main {
	static public function main() {
		new test.TestChaos();
		new test.TestOop();
		trace('Done! ${test.BaseTest.numTests} tests with ${test.BaseTest.numFailures} failures');
	}
}
