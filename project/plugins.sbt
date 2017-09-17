addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.27")

// for benchmark charts
libraryDependencies ++= Seq(
  "org.jfree" % "jfreechart" % "1.0.14",
  "com.typesafe.play" %% "play-json" % "2.4.10"
)