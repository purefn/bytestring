organization := "purefn"

name := "bytestring"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.2"

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
, "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.0-M1"
, "org.scalaz" %% "scalaz-effect" % "7.0.0-M1"
, "org.scalaz" %% "scalaz-iteratee" % "7.0.0-M1"
)

initialCommands in console := """
import scalaz._
import Scalaz._
import purefn.bytestring._
import purefn.bytestring.syntax._
import java.io._
"""
