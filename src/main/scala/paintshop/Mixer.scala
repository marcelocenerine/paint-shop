package paintshop

import scala.annotation.tailrec

object Mixer {

  /**
    * Time complexity: `O(2^n)` - exponential - in the worst case scenario, where n = sheens * colors.
    */
  def mix(selections: List[PaintSelection], palette: Palette): Option[PaintSelection] = {
    if (selections.isEmpty || !canPaletteFulfilSelections(selections, palette)) None
    else {
      val allCombinations = combinations(palette.colors, palette.sheens)
      val orderedByCost = allCombinations.sortBy(_.paints.count(_ == Matte))

      def satisfiesAll(mix: PaintSelection): Boolean = {
        def isHappy(asked: PaintSelection) = asked.paints.exists(mix.paints.contains(_))

        selections.forall(isHappy)
      }

      orderedByCost.find(satisfiesAll)
    }
  }

  private def canPaletteFulfilSelections(selections: List[PaintSelection], palette: Palette): Boolean =
    selections.forall(sel =>
      sel.paints.forall(paint => palette.colors.contains(paint.color) && palette.sheens.contains(paint.sheen)))

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
