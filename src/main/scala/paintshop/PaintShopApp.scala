package paintshop

import java.io.{File, FileInputStream, InputStream}

import scala.io.Source

object PaintShopApp extends App {

  if (args.length != 1) {
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
        case Right(BatchOrder(palette, selections)) =>
          Mixer.mix(selections, palette) match {
            case Some(mix) => println(mix)
            case None => println("No solution exists")
          }

        case Left(parseError) => println(parseError.msg)
      }
    } catch {
      case ex: Throwable =>
        println("Unexpected error: " + ex.getMessage)
        sys.exit(1)
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
