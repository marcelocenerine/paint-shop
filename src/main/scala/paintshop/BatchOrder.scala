package paintshop

import scala.io.Source
import scala.util.Try

class BatchOrder private (val palette: Palette, val selections: List[PaintSelection]) {
  def isEmpty: Boolean = selections.isEmpty
}

object BatchOrder {

  def unapply(order: BatchOrder): Option[(Palette, List[PaintSelection])] = Some((order.palette, order.selections))

  def empty: BatchOrder = new BatchOrder(Palette(Set.empty, Set.empty), Nil)

  def from(source: Source): Either[ParseError, BatchOrder] = {
    val lines = source.getLines().toList

    lines match {
      case header :: lines =>
        tryToInt(header) match {
          case Some(colorCount) if colorCount == 0 && lines.isEmpty => Right(BatchOrder.empty)
          case Some(colorCount) if colorCount > 0 =>
            if (!lines.isEmpty) {
              traverse(lines)(parseLine(colorCount, _)).map { selections =>
                val colors = ((1 to colorCount) map Color).toSet
                val palette = Palette(colors, Sheen.all)
                new BatchOrder(palette, selections)
              }
            } else Left(ParseError("Color count informed but no lines"))

          case _ => Left(ParseError(s"Invalid color count: '$header'"))
        }
      case Nil => Left(ParseError("Empty input file"))
    }
  }

  private val ValidLineRegex = """^(\d+ [GM])( \d+ [GM])*$""".r
  private val PairRegex = """(\d+)\s([GM])""".r

  private def parseLine(colorCount: Int, line: String): Either[ParseError, PaintSelection] = {
    line match {
      case ValidLineRegex(_*) =>
        val maybePaints = PairRegex.findAllMatchIn(line).map { matching =>
          val maybeColorId = tryToInt(matching.group(1))
          val maybeSheen = Sheen.from(matching.group(2))

          (maybeColorId, maybeSheen) match {
            case (Some(colorId), Some(sheen)) =>
              if (colorId > 0 && colorId <= colorCount) Right(Paint(Color(colorId), sheen))
              else Left(ParseError(s"Invalid color '$colorId' in line '$line'"))

            case _ => throw new Error("incorrect regex. Fix it!") // regex made sure pair of values are valid
          }
        }

        traverse(maybePaints.toList)(identity).map(PaintSelection)

      case _ => Left(ParseError(s"Malformed line: '$line'"))
    }
  }

  private def tryToInt(s: String): Option[Int] = Try(s.toInt).toOption

  private def traverse[A, L, R](xs: List[A])(op: A => Either[L, R]): Either[L, List[R]] =
    xs.foldRight(Right(Nil): Either[L, List[R]]) { (x, acc) =>
      for (axs <- acc.right; opx <- op(x).right) yield opx :: axs
    }

  case class ParseError(msg: String)
}
