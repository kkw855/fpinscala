name := "my_fpInScala"

version := "0.1"

scalaVersion := "2.13.5"

ThisBuild / useCoursier := false

idePackagePrefix := Some("com.endsoul.fp.scala")

// "% TEST" 를 붙이면 해당 라이브러리를 test 폴더에서만 사용 가능하다. main 폴더에서만 import 불가능.
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % Test

libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0" withSources () withJavadoc ()
libraryDependencies += "org.typelevel" %% "cats-kernel" % "2.1.0" withSources () withJavadoc ()
libraryDependencies += "org.typelevel" %% "cats-macros" % "2.1.0" withSources () withJavadoc ()

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.1.1" withSources () withJavadoc ()

// For Scala 2.13+
scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-encoding",
  "UTF-8",
  "-Xfatal-warnings",
  "-language:implicitConversions",
  "-language:postfixOps"
)
