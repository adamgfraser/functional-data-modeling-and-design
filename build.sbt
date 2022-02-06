lazy val root = project
  .in(file(""))
  .settings(
    name := "functional-data-modeling-and-design",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-language:postfixOps"
    ),
    scalaVersion := "2.13.8"
  )

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "2.0.0-RC2",
  "dev.zio" %% "zio-streams" % "2.0.0-RC2",
  "dev.zio" %% "zio-test" % "2.0.0-RC2" % "test"
)

testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
