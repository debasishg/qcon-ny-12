name := "qconny12"

organization := "net.debasishg"

version := "0.1"

scalaVersion := "2.9.1"

// crossScalaVersions := Seq("2.9.2", "2.9.1", "2.9.0", "2.8.1", "2.8.0")

// resolvers ++= Seq("Twitter Repository" at "http://maven.twttr.com", "Akka Repository" at "http://akka.io/repository")

resolvers ++= Seq("Akka Repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies <++= scalaVersion { scalaVersion =>
  Seq(
    "org.scala-lang.plugins"         % "continuations"   % scalaVersion,
    "net.debasishg"                 %% "redisclient"     % "2.4.2",
    "net.debasishg"                 %% "sjson"           % "0.15",
    "org.slf4j"                      % "slf4j-api"       % "1.6.1",
    "org.slf4j"                      % "slf4j-log4j12"   % "1.6.1"            % "provided",
    "log4j"                          % "log4j"           % "1.2.16"           % "provided",
    "junit"                          % "junit"           % "4.8.1"            % "test",
    "org.scalatest"                 %% "scalatest"       % "1.6.1"            % "test",
    "org.scalaz"                    %% "scalaz-core"     % "6.0.3",
    "com.typesafe.akka"              % "akka-actor"      % "2.0",
    "com.typesafe.akka"              % "akka-testkit"    % "2.0",
    "ch.qos.logback"                 % "logback-classic" % "0.9.28"           % "runtime"
  )
}

autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.1")

scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-Xcheckinit", "-P:continuations:enable")

parallelExecution in Test := false

publishTo := Some("Scala-Tools Nexus Repository for Releases" at "http://nexus.scala-tools.org/content/repositories/releases")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
