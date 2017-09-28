package paintshop

import java.io.{File, FileInputStream, InputStream}
import scala.concurrent.duration._
import scala.language.postfixOps
import paintshop.misc.TryInt

import scala.io.Source

object PaintShopApp extends App {

  if (args.length < 1) {
    println("Please inform path to the input file")
  } else {
    val path = args(0)
    locateFile(path) match {
      case Some(is) => processFile(is)
      case None => println(s"File not found: $path")
    }
  }

  private def processFile(is: InputStream): Unit = {
    try {
      val input = Source.fromInputStream(is)

      BatchOrder.from(input) match {
        case Right(BatchOrder(orders)) =>
          val mixer = chooseMixerAlgorithm
          val maybeSolution = mixer.mix(orders)
          printSolution(maybeSolution)

        case Left(parseError) => println(parseError.msg)
      }
    } catch {
      case ex: Throwable =>
        println("Unexpected error: " + ex.getMessage)
        sys.exit(1)
    }

    def chooseMixerAlgorithm = {
      args match {
        case Array(_, "--tabu-search", _*) =>
          val duration = (args match {
            case Array(_, _, duration, _*) => duration.toIntOption
            case _ => None
          }).getOrElse(5) // 5 seconds by default
          new TabuSearchMixer(duration seconds)

        case _ => BruteForceMixer
      }
    }

    def printSolution(solution: Option[PaintMix]) = solution match {
      case Some(mix) =>
        val sortedPaints = mix.paints.toSeq.sorted
        println(sortedPaints.map(_.sheen).mkString(" "))

      case None => println("No solution exists")
    }
  }

  private def locateFile(path: String): Option[InputStream] = {
    val file = new File(path)
    Option {
      if (file.isFile) new FileInputStream(file)
      else getClass.getResourceAsStream(path)
    }
  }
}
