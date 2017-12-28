name := "libexp"

organization := "de.uni-ulm"

organizationName := "Ulm University, Institute of Aritificial Intelligence"

organizationHomepage := Some(url("http://www.uni-ulm.de/en/in/ki.html"))

/*
1.3: - change the old Exp parsers
1.4: - add facility to measure spent CPU time for old exp.
1.4.1: - remove many dependencies; rewrite parser transformer code without shapeless
2.0.0: new configuration framework
3.0.1: move to new dependencies (also scalaz -> cats);
       remove all old stuff, move some classes around (from exp.node.applicative to exp.node)
 */
version := "4.0.0-DEV"

description := "A Scala library for describing computational experiments"

homepage := Some(url("https://github.com/uulm-ai/libexp"))

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "0.4.2",
  "org.typelevel" %% "cats" % "0.9.0",
  "org.typelevel" % "cats-free_2.12" % "0.9.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.slf4j" % "slf4j-simple" % "1.7.12"
)

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.8" % "test"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

// if your project uses multiple Scala versions, use this for cross building
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary)

// if your project uses both 2.10 and polymorphic lambdas
libraryDependencies ++= (scalaBinaryVersion.value match {
  case "2.10" =>
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full) :: Nil
  case _ =>
    Nil
})
