package paintshop

import scala.io.Source
import paintshop.misc.traverse
import paintshop.misc.TryInt

class BatchOrder private (val selections: List[PaintSelection]) {
  def isEmpty: Boolean = selections.isEmpty
}

object BatchOrder {

  def unapply(order: BatchOrder): Option[List[PaintSelection]] = Some(order.selections)

  def empty: BatchOrder = new BatchOrder(Nil)

  def from(source: Source): Either[ParseError, BatchOrder] = {
    val lines = source.getLines().toList

    lines match {
      case header :: orders => parseOrders(header, orders)
      case Nil => Left(ParseError("Empty input file"))
    }
  }

  private def parseOrders(header: String, lines: List[String]) = {
    header.toIntOption match {
      case Some(colorCount) if colorCount == 0 && lines.isEmpty => Right(BatchOrder.empty)
      case Some(colorCount) if colorCount > 0 =>
        if (!lines.isEmpty) {
          traverse(lines)(parseOrder(colorCount, _)).flatMap { selections =>
            val distinctColors = (for ( sel <- selections; p <- sel.paints ) yield p.color).distinct

            if (distinctColors.size == colorCount) Right(new BatchOrder(selections))
            else Left(ParseError(s"Total number of colors < $colorCount"))
          }
        } else Left(ParseError("Color count informed but no orders"))

      case _ => Left(ParseError(s"Invalid color count: '$header'"))
    }
  }

  private val OrderRegex = """^(\d+ [A-Z])( \d+ [A-Z])*$""".r

  private def parseOrder(colorCount: Int, line: String): Either[ParseError, PaintSelection] = {
    line match {
      case OrderRegex(_*) =>
        val pairs = line.split(" ").grouped(2) // regex made sure all are pairs
        val maybePaints = traverse(pairs){ case Array(c, s) =>
          (c.toIntOption, Sheen.from(s)) match {
            case (Some(colorId), Some(sheen)) if colorId > 0 && colorId <= colorCount => Right(Paint(Color(colorId), sheen))
            case (_, Some(_)) => Left(ParseError(s"Invalid color '$c' in line '$line'"))
            case (_, None) => Left(ParseError(s"Invalid sheen '$s' in line '$line'"))
          }
        }
        maybePaints.flatMap { paints =>
          val distinctColors = paints.map(_.color).distinct

          if (paints.size == distinctColors.size) Right(PaintSelection(paints.toSet))
          else Left(ParseError(s"Duplicate colors in line '$line'"))
        }

      case _ => Left(ParseError(s"Malformed line: '$line'"))
    }
  }

  case class ParseError(msg: String)
}
