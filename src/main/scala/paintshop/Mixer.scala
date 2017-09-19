package paintshop

object Mixer {

  def mix(selections: List[PaintSelection]): Option[PaintSelection] = {
    if (selections.isEmpty) None
    else {
      val distinctColors = (for ( s <- selections; p <- s.paints) yield p.color).toSet

      if (distinctColors.isEmpty) Some(PaintSelection(Set.empty))
      else {
        val sheensByIncreasingCost = Sheen.all.toSeq.sorted

        def satisfiesAll(mix: Set[Paint]): Boolean = {
          def isHappy(asked: PaintSelection) = asked.paints.exists(mix.contains(_))

          selections.forall(isHappy)
        }

        findOptimalSelection(distinctColors, sheensByIncreasingCost, satisfiesAll)
      }
    }
  }

  /**
    * Lazily explores the search space using exhaustive search (brute-force). It expects the `sheens` to be ordered
    * by increasing cost, so that the cheapest combinations are evaluated first. This also guarantees that the first
    * feasible solution found is also one of the optimal solutions.
    *
    * The time complexity of this algorithm is `O(n^m)` - exponential - in the worst case scenario, where n = sheens
    * and m = colors.
    *
    * This method is not tail recursive. This is an intentional design decision that trades-off memory allocation
    * vs being stack-friendly. This, however, is unlikely to be an issue as the max stack depth will be equal
    * to `colors.size`.
    *
    * @param colors - colors available in the palette.
    * @param sheens - sheens available in the palette sorted by increasing cost.
    * @param p - predicate that returns true if a given solution is feasible.
    * @return the optimal solution that satisfies the predicate `p`. None if no feasible solution is found.
    */
  private def findOptimalSelection(colors: Set[Color], sheens: Seq[Sheen], p: Set[Paint] => Boolean): Option[PaintSelection] = {
    def comb(currentColors: List[Color], partialComb: Set[Paint]): Option[PaintSelection] = {
      currentColors match {
        case head :: tail =>
          (for {
            sheen <- sheens.toStream // lazily explores the combinations
            sel <- comb(tail, partialComb + Paint(head, sheen))
          } yield sel).headOption // stops at the (possible) optimal solution

        case Nil => if (p(partialComb)) Some(PaintSelection(partialComb)) else None
      }
    }

    comb(colors.toList, Set.empty)
  }
}
