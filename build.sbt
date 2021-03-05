name := "my_fpInScala"

version := "0.1"

scalaVersion := "2.13.5"

idePackagePrefix := Some("com.endsoul.fp.scala")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"

libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0" withSources () withJavadoc ()
libraryDependencies += "org.typelevel" %% "cats-kernel" % "2.1.0" withSources () withJavadoc ()
libraryDependencies += "org.typelevel" %% "cats-macros" % "2.1.0" withSources () withJavadoc ()

// For Scala 2.13+
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-Xfatal-warnings",
  "-feature",
  "-language:implicitConversions"
)
