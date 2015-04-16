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
version := "2.0.0-DEV"

description := "A Scala library for describing computational experiments"

homepage := Some(url("https://github.com/uulm-ai/libexp"))

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

scalaVersion := "2.11.5"

libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.1"

libraryDependencies += "com.github.scopt" % "scopt_2.11" % "3.2.0"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0"

libraryDependencies += "org.specs2" % "specs2_2.11" % "2.3.12" % "test"

resolvers += "mvn@mirkwood" at "http://mirkwood.informatik.uni-ulm.de/mvn"

libraryDependencies += "de.uni-ulm" %% "vultura-util" % "23.1.1"

libraryDependencies += "org.apache.spark" %% "spark-core" % "1.3.0"

