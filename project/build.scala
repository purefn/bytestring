import sbt._
import Keys._

object build extends Build {
  type Sett = Project.Setting[_]

  val scalazVersion = "7.0.0-M1"

  lazy val standardSettings = Defaults.defaultSettings ++ List[Sett](
    organization := "org.purefn"
  , scalaVersion := "2.9.2"
  , crossScalaVersions := List("2.9.2", "2.10.0-M5")
  , crossVersion := CrossVersion.full
  , resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases"
  , scalacOptions <++= (scalaVersion).map((sv: String) => List("-deprecation", "-unchecked", "-Ywarn-value-discard") ++ (if(sv.contains("2.10")) None else Some("-Ydependent-method-types")))
  , scalacOptions in (Compile, doc) <++= (baseDirectory in LocalProject("bytestring")).map {
      bd => List("-sourcepath", bd.getAbsolutePath, "-doc-source-url", "https://github.com/purefn/bytestring/€{FILE_PATH}.scala")
    }
  )

  lazy val bytestring = Project(
    id = "bytestring"
  , base = file(".")
  , settings = standardSettings ++ Unidoc.settings ++ List[Sett](
      name := "bytestring"
    , libraryDependencies ++= List(
        "org.scalaz" %% "scalaz-core" % scalazVersion cross CrossVersion.full
      , "org.scalaz" %% "scalaz-effect" % scalazVersion cross CrossVersion.full
      , "org.scalaz" %% "scalaz-iteratee" % scalazVersion cross CrossVersion.full
      )
  )
  , aggregate = List(scalacheckBinding, tests)
  )

  lazy val scalacheckBinding: Project = Project(
    id           = "scalacheck-binding"
  , base         = file("scalacheck-binding")
  , dependencies = List(bytestring)
  , settings     = standardSettings ++ List[Sett](
      name := "bytestring-scalacheck-binding"
    , libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" cross CrossVersion.full
    )
  )

  lazy val tests: Project = Project(
    id           = "tests"
  , base         = file("tests")
  , dependencies = List(bytestring, scalacheckBinding % "test")
  , settings     = standardSettings ++ List[Sett](
      name := "bytestring-tests"
    , libraryDependencies ++= List(
        "org.specs2" %% "specs2" % "1.11" % "test"
      , "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
      )
    )
  )
}

