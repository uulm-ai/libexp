name := "libexp"

organization := "de.uni-ulm"

/*
1.3: - change the old Exp parsers
1.4: - add facility to measure spent CPU time for old exp.
1.4.1: - remove many dependencies; rewrite parser transformer code without shapeless
 */
version := "1.4.1"

scalaVersion := "2.10.4"

libraryDependencies += "org.specs2" % "specs2_2.10" % "2.0" % "test"