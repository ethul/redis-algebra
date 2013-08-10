name := "redis-algebra"

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0-SNAPSHOT"

libraryDependencies += "typeclass-inject" %% "typeclass-inject" % "0.0.1"

libraryDependencies += "org.specs2" %% "specs2" % "2.0" % "test"

resolvers += "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Ethul repository snapshots" at "https://github.com/ethul/ivy-repository/raw/master/snapshots/"

resolvers += "Ethul repository releases" at "https://github.com/ethul/ivy-repository/raw/master/releases/"

scalacOptions += "-feature"

scalacOptions += "-language:higherKinds"

scalacOptions += "-Xlint"

scalacOptions += "-Xfatal-warnings"

scalacOptions += "-Yno-adapted-args"

scalacOptions += "-Ywarn-all"
