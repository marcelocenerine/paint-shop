package paintshop

import scala.annotation.tailrec

object Mixer {

  /**
    * Time complexity: `O(n^m)` - exponential - in the worst case scenario, where n = sheens and m = colors.
    */
  def mix(selections: List[PaintSelection]): Option[PaintSelection] = {
    if (selections.isEmpty) None
    else {
      val distinctColors = (for ( s <- selections; p <- s.paints) yield p.color).toSet
      val allCombinations = combinations(distinctColors, Sheen.all)
      val orderedByCost = allCombinations.sortBy(_.paints.count(_ == Matte))

      def satisfiesAll(mix: PaintSelection): Boolean = {
        def isHappy(asked: PaintSelection) = asked.paints.exists(mix.paints.contains(_))

        selections.forall(isHappy)
      }

      orderedByCost.find(satisfiesAll)
    }
  }

  private def combinations(colors: Set[Color], sheens: Set[Sheen]): List[PaintSelection] = {
    @tailrec
    def comb(currentColors: List[Color], partialCombs: List[Set[Paint]]): List[PaintSelection] = {
      currentColors match {
        case head :: tail =>
          val newPartialCombs = for {
            comb <- partialCombs
            sheen <- sheens
          } yield comb + Paint(head, sheen)
          comb(tail, newPartialCombs)

        case Nil => partialCombs.map(PaintSelection)
      }
    }

    comb(colors.toList, List(Set.empty))
  }
}
