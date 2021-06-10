name := "my_fpInScala"

version := "0.1"

scalaVersion := "2.13.5"

ThisBuild / useCoursier := false

idePackagePrefix := Some("com.endsoul.fp.scala")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"

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
