organization := "purefn"

name := "bytestring"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.2"

scalacOptions ++= List("-unchecked", "-deprecation", "-Ywarn-value-discard")

resolvers ++= List(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
, "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.0.0-M1"
, "org.scalaz" %% "scalaz-effect" % "7.0.0-M1"
, "org.scalaz" %% "scalaz-iteratee" % "7.0.0-M1"
, "org.specs2" %% "specs2" % "1.12" % "test"
, "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
)

initialCommands in console := """
import scalaz._
import Scalaz._
import purefn.bytestring._
import purefn.bytestring.syntax._
import java.io._
"""
