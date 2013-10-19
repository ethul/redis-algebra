organization := "com.github.ethul"

name := "redis-algebra"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0-M3"

libraryDependencies += "org.specs2" %% "specs2" % "2.2.2-scalaz-7.1.0-M3" % "test"

resolvers += "Sonatype releases" at "http://oss.sonatype.org/content/repositories/releases/"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

scalacOptions += "-feature"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-language:higherKinds"

scalacOptions += "-Xlint"

scalacOptions += "-Xfatal-warnings"

scalacOptions += "-Yno-adapted-args"

scalacOptions += "-Ywarn-all"

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
