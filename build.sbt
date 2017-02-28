name := "libexp"

organization := "de.uni-ulm"

organizationName := "Ulm University, Institute of Aritificial Intelligence"

organizationHomepage := Some(url("http://www.uni-ulm.de/en/in/ki.html"))

/*
1.3: - change the old Exp parsers
1.4: - add facility to measure spent CPU time for old exp.
1.4.1: - remove many dependencies; rewrite parser transformer code without shapeless
2.0.0: new configuration framework
3.0.1: move to new dependencies (also scalaz -> cats)
 */
version := "3.0.1-DEV"

description := "A Scala library for describing computational experiments"

homepage := Some(url("https://github.com/uulm-ai/libexp"))

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "0.4.2",
  "com.chuusai" %% "shapeless" % "2.3.2",
//  "io.reactivex" %% "rxscala" % "0.25.0",
  "org.typelevel" %% "cats" % "0.9.0",
  "org.typelevel" % "cats-free_2.12" % "0.9.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "org.slf4j" % "slf4j-simple" % "1.7.12",
  "org.apache.commons" % "commons-math3" % "3.5"
)

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.8" % "test"
