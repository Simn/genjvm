package test;
import haxe.macro.Expr;

enum A<T> {
	TA<Q>(q : Q) : A<Q>;
	TB(v : Bool) : A<Bool>;
	TC(v : Bool) : A<String>;
}

class TestMatch extends BaseTest {
	public function new() {
		super();
		testBasic();
		testTuple();
		testGrouping();
	}

	static function switchNormal(e:Expr):String {
		return switch(e.expr) {
			case EConst(CString(s)): s;
			case EParenthesis( { expr : EConst(CString(s)) } )
			| EUntyped( { expr : EConst(CString(s)) } ):
				s;
			case EField(_, s):
				s;
			case EArray(_, { expr : EConst(CInt(i) | CFloat(i)) } ):
				Std.string(i);
			case EBinop(OpIn, _, { expr : e, pos : _ }) :
				Std.string(e);
			case _:
				"not_found";
		}
	}

	static function switchCapture(e:Expr) {
		return switch(e) {
			case { expr : EConst(const = (CString("foobar") | CInt("9"))) } :
				const;
			case _:
				null;
		}
	}

	static function switchArray(e:Expr):String {
		return switch(e.expr) {
			case EArrayDecl([]):
				"[]";
			case EArrayDecl([a]):
				"[" + Std.string(a.expr) + "]";
			case EArrayDecl([a,b]):
				"[" + Std.string(a.expr) + "," + Std.string(b.expr) + "]";
			case _:
				"_";
		}
	}

	static function switchArray2(a:Array<String>):String {
		return switch(a) {
			case ["a", "b"]: "0";
			case ["a"]: "1";
			case ["b"]: "2";
			case [a]: "3:" + a;
			case [a, b]: "4:" + a + "," +b;
			case var a if (a.length == 3): "5:" + a.length;
			case []: "6";
			case _: "7";
		}
	}

	static function switchStructure(a: { foo:String, bar:String } ) {
		return switch(a) {
			case { foo: "val1", bar:"val2" } : "0";
			case { foo: "val1" } : "1";
			case { bar: "val2" } : "2";
			case { bar: a } : a;
		}
	}

	static function switchCrazy(e:Expr) {
		return switch(e.expr) {
			case EUntyped( { expr : EParenthesis( { expr : EArray( { expr: a = EConst(CString(_)) }, { expr : EConst(CInt(b)) } ) } ) } ):
				Std.string(a) + ":" +b;
			case _:
				"_";
		}
	}

	static function switchGuard(e:Expr):String {
		return switch(e.expr) {
			case EConst(CString(s)) if (StringTools.startsWith(s, "foo")):
				"1";
			case EConst(CString(s)) if (StringTools.startsWith(s, "bar")):
				"2";
			case EConst(CInt(i)) if (switch(Std.parseInt(i) * 2) { case 4: true; case _: false; }):
				"3";
			case EConst(_):
				"4";
			case _:
				"5";
		}
	}

	function testBasic() {
		eq("bar", switchNormal(macro "bar"));
		eq("bar", switchNormal(macro ("bar")));
		eq("bar", switchNormal(macro untyped "bar"));
		eq("foo", switchNormal(macro null.foo));
		eq("22", switchNormal(macro null[22]));
		eq("22.5", switchNormal(macro null[22.5]));
		eq("EConst(CInt(0))", switchNormal(macro 1 in 0));
		eq("not_found", switchNormal(macro null["22"]));

		t(null != switchCapture(macro "foobar"));
		t(null == switchCapture(macro "fooba"));
		t(null != switchCapture(macro 9));
		t(null == switchCapture(macro 10));

		eq("[]", switchArray(macro []));
		eq("_", switchArray(macro 2));
		eq("[EConst(CInt(22))]", switchArray(macro [22]));
		eq("[EConst(CInt(22)),EConst(CString(foo))]", switchArray(macro [22,"foo"]));
		eq("_", switchArray(macro [22, "foo", "bar"]));

		eq("0", switchArray2(["a", "b"]));
		eq("1", switchArray2(["a"]));
		eq("2", switchArray2(["b"]));
		eq("3:c", switchArray2(["c"]));
		eq("4:a,a", switchArray2(["a","a"]));
		eq("4:b,a", switchArray2(["b","a"]));
		eq("5:3", switchArray2(["a","a","a"]));
		eq("6", switchArray2([]));
		eq("7", switchArray2(["a", "a", "a", "b"]));

		eq("EConst(CString(foobar)):12", switchCrazy(macro untyped ("foobar"[12])));

		// eq("1", switchGuard(macro "foobar"));
		// eq("2", switchGuard(macro "barfoo"));
		// eq("3", switchGuard(macro 2));
		// eq("4", switchGuard(macro 5));
		// eq("4", switchGuard(macro "bazfoo"));
		// eq("5", switchGuard(macro []));

		eq("0", switch ([true, 1, "foo"]) {
			case [true, 1, "foo"]: "0";
			case [true, 1, _]: "1";
			case _: "_";
		});

		eq("0", switch [true, 1, "foo"] {
			case [true, 1, "foo"]: "0";
			case [true, 1, _]: "1";
			case _: "_";
		});

		eq("1", switch [true, 1, "bar"] {
			case [true, 1, "foo"]: "0";
			case [true, 1, _]: "1";
			case _: "_";
		});

		eq("_", switch [false, 1, "foo"] {
			case [true, 1, "foo"]: "0";
			case [true, 1, _]: "1";
			case _: "_";
		});

		eq("1", switch [1, 2] {
			case [0, 0] | [1, 2]: "1";
			case [1, 1]: "2";
			case _: "_";
		});

		var t = TA("foo");
		eq("0", switch(t) {
			case TA("foo"): "0";
			case TA(_): "1";
			case TC(_): "2";
		});
	}

	function testTuple() {
		function test(a:Int, b:Int, c:Int) return switch [a, b, c] {
			case [x, 1, 2] | [1, 2, x] | [1, x, 2]: '0|x:$x';
			case [3, 4, z] | [z, 3, 4] | [3, z, 4]: '1|z:$z';
			case [1, y, z] | [2, z, y]: '2|y:$y,z:$z';
			case [x, y, z]: '_:x:$x,y:$y,z:$z';
		}
		eq("0|x:9", test(9, 1, 2));
		eq("0|x:9", test(1, 2, 9));
		eq("0|x:9", test(1, 9, 2));
		eq("1|z:12", test(3, 4, 12));
		eq("1|z:12", test(12, 3, 4));
		eq("1|z:12", test(3, 12, 4));
		eq("2|y:9,z:8", test(1, 9, 8));
		eq("2|y:9,z:8", test(2, 8, 9));
		eq("_:x:9,y:8,z:7", test(9, 8, 7));
	}

	function testGrouping() {
		function test(v) return switch(v) {
			case 1, 2, 3: "0";
			case val = (4 | 5 | 6) if (val == 5): "1";
			case 4, 5, 6: "2";
			case 8, 9: "3";
			case var x: '_:$x';
		}
		var results = ["_:0", "0", "0", "0", "2", "1", "2", "_:7", "3", "3", "_:10"];
		for (i in 0...results.length) {
			eq(results[i], test(i));
		}
	}
}