ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.17"

lazy val root = (project in file("."))
  .settings(
    name := "nm_metric_evaluations",
    idePackagePrefix := Some("ca.mikelavender.nm_metric_evaluations"),
    assembly / mainClass := Some("ca.mikelavender.nm_metric_evaluations.Main"),
    assembly / assemblyJarName := "nm_tests.jar"
  )

libraryDependencies ++= Seq(
  "org.apache.commons"    % "commons-compress"              % "1.23.0",
  "org.apache.commons"    % "commons-lang3"                 % "3.12.0",
  "commons-io"            % "commons-io"                    % "2.11.0",
  "org.scalanlp"          %% "breeze"                       % "2.1.0"
)

libraryDependencies += "ca.mikelavender" % "associationstestlibrary_2.12" % "1.1.0-SNAPSHOT" from
  "file:/Users/mikelavender/.ivy2/local/ca.mikelavender/associationtestslibrary_2.12/1.1.0-SNAPSHOT/jars/associationtestslibrary_2.12.jar"


