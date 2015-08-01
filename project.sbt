
organization in Global := "oncue.quiver"

scalaVersion in Global := crossScalaVersions.value.head

crossScalaVersions in Global := Seq("2.11.7", "2.10.4")

scalacOptions in Global ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

lazy val quiver = project.in(file(".")).aggregate(core,codecs,json,docs)

lazy val core = project

lazy val docs = project.dependsOn(core, codecs)

lazy val codecs = project.dependsOn(core % "test->test;compile->compile")

lazy val json = project.dependsOn(core % "test->test;compile->compile")

releaseCrossBuild := true

publishArtifact in (Compile, packageBin) := false

publish := ()

publishLocal := ()
