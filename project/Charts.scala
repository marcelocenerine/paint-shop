package paintshop.benchmark

import javax.imageio.ImageIO

import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.LogarithmicAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYErrorRenderer
import org.jfree.data.xy.{YIntervalSeries, YIntervalSeriesCollection}
import play.api.libs.json._
import sbt._

object Charts {

  def apply(jmhReport: File, targetDir: File): Unit = {
    val json = Json.parse(IO.read(jmhReport))

    json.as[List[JsObject]]
      .groupBy(result => (result \ "benchmark").as[String])
      .foreach { case (benchmark, iterations) =>
        val ySeries = new YIntervalSeries(benchmark)
        for (iteration <- iterations) {
          ySeries.add(
            (iteration \ "params" \ "colors").as[String].toDouble,
            (iteration \ "primaryMetric" \ "score").as[Double],
            (iteration \ "primaryMetric" \ "scoreConfidence").apply(0).as[Double],
            (iteration \ "primaryMetric" \ "scoreConfidence").apply(1).as[Double]
          )
        }

        val xAxis = new LogarithmicAxis("Colors")
        xAxis.setAllowNegativesFlag(true)
        val yAxis = new LogarithmicAxis("Execution time (op/us)")
        yAxis.setAllowNegativesFlag(true)

        val col = new YIntervalSeriesCollection()
        col.addSeries(ySeries)
        val renderer = new XYErrorRenderer
        renderer.setSeriesLinesVisible(0, true)

        val plot = new XYPlot(col, xAxis, yAxis, renderer)
        val chart = new JFreeChart(benchmark, JFreeChart.DEFAULT_TITLE_FONT, plot, true)
        ImageIO.write(chart.createBufferedImage(800, 600), "png", targetDir / s"$benchmark.png")
      }
  }
}