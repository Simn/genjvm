package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
using StringTools;

class Java {
	static public function getJavaDependencies() {
		haxelibInstallGit("HaxeFoundation", "hxjava", true);
		runCommand("javac", ["-version"]);
	}

	static public function run(args:Array<String>) {
		getJavaDependencies();
		changeDirectory(genjvmDir);
		runCommand("haxe", ["build.hxml"]);
		runCommand("java", ["-jar", "export/Main.jar"]);
	}
}