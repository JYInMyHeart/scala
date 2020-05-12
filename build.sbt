name := "scala"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.26.5"

libraryDependencies += "io.reactivex.rxjava2" % "rxjava" % "2.2.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
// https://mvnrepository.com/artifact/io.netty/netty-all
libraryDependencies += "io.netty" % "netty-all" % "4.1.6.Final"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
libraryDependencies ++= Seq("com.chuusai" %% "shapeless" % "2.3.3")
// https://mvnrepository.com/artifact/org.scalaz/scalaz-core
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0-M31"
// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-parser-combinators
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
// https://mvnrepository.com/artifact/com.alibaba/fastjson
libraryDependencies += "com.alibaba" % "fastjson" % "1.2.68"
