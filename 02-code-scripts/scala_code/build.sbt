ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.3"

lazy val root = (project in file("."))
  .settings(
    name := "nm_metrics_distributions"
  )

libraryDependencies ++= Seq(
  "org.apache.commons"    % "commons-compress"             % "1.19",
  "commons-io"            % "commons-io"                   % "2.5",
  "ca.mikelavender"       % "associationtestslibrary_2.12" % "1.1.0-SNAPSHOT"
)

//libraryDependencies += "ca.mikelavender" % "associationtestslibrary_2.12" % "1.1.0-SNAPSHOT" from
//  "file:/Users/mikelavender/.ivy2/local/ca.mikelavender/associationtestslibrary_2.12/1.1.0-SNAPSHOT/jars/associationtestslibrary_2.12.jar"