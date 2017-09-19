name := "paint-shop"

version := "1.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.1",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

lazy val root = (project in file(".")).settings(
  commonSettings,
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  )
)

lazy val benchmark = project.in(file("benchmark"))
  .dependsOn(root)
  .enablePlugins(JmhPlugin)
  .settings(
    commonSettings,
    charts := Def.inputTaskDyn {
      val benchmarks = Def.spaceDelimited().parsed
      val targetDir = crossTarget.value
      val jmhReport = targetDir / "jmh-result.json"
      val runTask = run in Jmh
      Def.inputTask {
        val _ = runTask.evaluated
        paintshop.benchmark.Charts(jmhReport, targetDir)
        targetDir
      }.toTask(s" -rf json -rff ${jmhReport.absolutePath} ${benchmarks.mkString(" ")}")
    }.evaluated
  )

lazy val charts = inputKey[File]("Runs the benchmarks and produce charts")