package object paintshop {

  case class Color(id: Int)

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

  case class Paint(color: Color, sheen: Sheen)

  case class PaintSelection(paints: List[Paint])

  case class Palette(colors: Set[Color], sheens: Set[Sheen])
}