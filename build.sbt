name := "libexp"

organization := "de.uni-ulm"

organizationName := "Ulm University, Institute of Aritificial Intelligence"

organizationHomepage := Some(url("http://www.uni-ulm.de/en/in/ki.html"))

/*
1.3: - change the old Exp parsers
1.4: - add facility to measure spent CPU time for old exp.
1.4.1: - remove many dependencies; rewrite parser transformer code without shapeless
2.0.0: new configuration framework
 */
version := "3.0.0-HPOMDP"

description := "A Scala library for describing computational experiments"

homepage := Some(url("https://github.com/uulm-ai/libexp"))

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

scalaVersion := "2.11.7"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.2.1"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.6.4" % "test"

libraryDependencies += "de.uni-ulm" %% "vultura-util" % "23.2.0"

resolvers += "mvn@mirkwood" at "http://mirkwood.informatik.uni-ulm.de/mvn"
