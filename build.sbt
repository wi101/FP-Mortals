name := "fp-mortals"
version := "0.1"
scalaVersion := "2.12.6"


scalacOptions in ThisBuild ++= Seq(
  "-language:_",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)
libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.13.0",
  "org.scalaz"           %% "scalaz-core" % "7.2.26"
)
