name := "rpeanut-addiction"

version := "1.0"

scalaVersion := "2.11.8"

resolvers ++= List(
  Resolver.sonatypeRepo("releases"),
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "3.0.0",
  "com.lihaoyi" %% "fastparse" % "0.4.1",
  "org.scalacheck" %% "scalacheck" % "1.13.2",
  "com.softwaremill.quicklens" %% "quicklens" % "1.4.8",
  "org.scalactic" %% "scalactic" % "3.0.0"
)

