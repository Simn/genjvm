<p align="center">
  <a href="https://haxe.org/" title="haxe.org"><img src="extra/images/Readme.png" /></a>
</p>

<p align="center">
	<a href="https://travis-ci.org/HaxeFoundation/haxe"><img src="https://travis-ci.org/HaxeFoundation/haxe.svg?branch=development" alt="TravisCI Build Status"></a>
	<a href="https://ci.appveyor.com/project/HaxeFoundation/haxe"><img src="https://ci.appveyor.com/api/projects/status/github/HaxeFoundation/haxe?branch=development&amp;svg=true" alt="AppVeyor Build Status"></a>
	<a href="https://saucelabs.com/u/haxe"><img src="https://saucelabs.com/buildstatus/haxe" alt="SauceLabs Test Status"></a>
	<a href="https://gitter.im/HaxeFoundation/haxe?utm_source=badge&amp;utm_medium=badge&amp;utm_campaign=pr-badge"><img src="https://badges.gitter.im/Join%20Chat.svg" alt="Gitter"></a>
	<a href="https://discordapp.com/invite/0uEuWH3spjck73Lo"><img src="https://img.shields.io/discord/162395145352904705.svg?logo=discord" alt="Discord"></a>
</p>

---

## Genjvm

JVM target generator for Haxe. Use `-D jvm` with normal Java compilation parameters.

### Status

* Passes the unit tests and most other tests
* Still plenty of annoying corner cases to deal with
* Not in Haxe proper because I already have enough other stuff to maintain

### Differences to genjava

* Compiles much faster because there's no `javac` involved
* Performs equally in most cases, slower in some
* Type parameters work sometimes
* Cannot use any feature that deals with Java code, such as `__java__` or `@:functionCode`