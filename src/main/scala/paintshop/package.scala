package paintshop

case class Color(id: Int) extends Ordered[Color] {
  override def toString: String = id.toString
  override def compare(that: Color): Int = this.id.compare(that.id)
}

sealed trait Sheen {
  val id: String
  override def toString: String = id
}

case object Gloss extends Sheen {
  override val id = "G"
}

case object Matte extends Sheen {
  override val id = "M"
}

object Sheen {
  lazy val all: Set[Sheen] = Set(Gloss, Matte)
  def from(id: String): Option[Sheen] = all.find(_.id == id)
}

case class Paint(color: Color, sheen: Sheen) extends Ordered[Paint] {
  override def toString: String = s"$color $sheen"
  override def compare(that: Paint): Int = (this.color, this.sheen.id) compare (that.color, that.sheen.id)
}

case class PaintSelection(paints: Set[Paint])



package object misc {

  implicit class TryInt(s: String) {
    def toIntOption: Option[Int] = scala.util.Try(s.toInt).toOption
  }

  def traverse[A, L, R](xs: TraversableOnce[A])(op: A => Either[L, R]): Either[L, List[R]] =
    xs.foldRight(Right(Nil): Either[L, List[R]]) { (x, acc) =>
      for (axs <- acc; opx <- op(x)) yield opx :: axs
    }
}