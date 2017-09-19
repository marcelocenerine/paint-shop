package paintshop

import scala.annotation.tailrec

object Mixer {

  def mix(selections: List[PaintSelection]): Option[PaintSelection] = {
    if (selections.isEmpty) None
    else {
      val distinctColors = (for ( s <- selections; p <- s.paints) yield p.color).toSet

      if (distinctColors.isEmpty) Some(PaintSelection(Set.empty))
      else {
        def satisfiesAll(mix: Set[Paint]): Boolean = {
          def isHappy(asked: PaintSelection) = asked.paints.exists(mix.contains(_))

          selections.forall(isHappy)
        }

        val solutions = findFeasibleSolutions(distinctColors, Sheen.all, satisfiesAll)
        solutions.sortBy(sel => sel.paints.foldRight(0)((p, acc) => acc + p.sheen.cost)).headOption
      }
    }
  }

  /**
    * Explores the search space using exhaustive search (brute-force). The time complexity of this algorithm is
    * `O(n^m)` - exponential - in the worst case scenario, where n = sheens and m = colors.
    *
    * @param colors - colors available in the palette
    * @param sheens - sheens available in the palette
    * @param p - predicate that returns true if a given solution is feasible
    * @return a list of feasible solutions or empty if none found
    */
  private def findFeasibleSolutions(colors: Set[Color], sheens: Set[Sheen], p: Set[Paint] => Boolean): List[PaintSelection] = {
    @tailrec
    def combs(currentColors: List[Color], partialCombs: List[Set[Paint]]): List[PaintSelection] = {
      currentColors match {
        case head :: tail =>
          val newPartialCombs = for {
            comb <- partialCombs
            sheen <- sheens
          } yield comb + Paint(head, sheen)
          combs(tail, newPartialCombs)

        case Nil => partialCombs.filter(p).map(PaintSelection)
      }
    }

    combs(colors.toList, List(Set.empty))
  }
}
