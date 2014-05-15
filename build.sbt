name := "libexp"

organization := "de.uni-ulm"

/*
1.3: - change the old Exp parsers
1.4: - add facility to measure spent CPU time for old exp.
 */
version := "1.4"

scalaVersion := "2.10.4"

libraryDependencies += "com.chuusai" % "shapeless_2.10.0" % "1.2.4"

libraryDependencies += "org.scalaz" % "scalaz-core_2.10" % "7.0.1"

libraryDependencies += "org.specs2" % "specs2_2.10" % "2.0" % "test"

