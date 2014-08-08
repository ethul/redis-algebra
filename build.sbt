organization := "com.github.ethul"

name := "redis-algebra"

version := "0.2.0-SNAPSHOT"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.10.4", "2.11.2")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

libraryDependencies += "org.specs2" %% "specs2" % "2.4" % "test"

resolvers += "Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases/"

scalacOptions += "-feature"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-language:higherKinds"

scalacOptions += "-Xlint"

scalacOptions += "-Xfatal-warnings"

scalacOptions += "-Yno-adapted-args"

publishTo <<= version.apply { v =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("Snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("Releases" at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

pomIncludeRepository := Function.const(false)

pomExtra :=
  <url>https://github.com/ethul/redis-algebra</url>
  <licenses>
    <license>
      <name>MIT</name>
      <url>http://www.opensource.org/licenses/mit-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/ethul/redis-algebra</url>
    <connection>scm:git:git@github.com:ethul/redis-algebra.git</connection>
    <developerConnection>scm:git:git@github.com:ethul/redis-algebra.git</developerConnection>
  </scm>
  <developers>
    <developer>
      <id>ethul</id>
      <name>Eric Thul</name>
      <url>https://github.com/ethul</url>
    </developer>
  </developers>
