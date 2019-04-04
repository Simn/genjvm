class Main {
	static public function main() {
		utest.UTest.run([
			new test.TestChaos(),
			new test.TestOop(),
			new test.TestMatch(),
			new test.TestExceptions(),
			new test.TestClosure(),
			new test.TestTypeApi(),
			new test.TestReflectApi(),
			new test.TestStdApi(),
		]);
		test.TestControlFlow;
	}
}
