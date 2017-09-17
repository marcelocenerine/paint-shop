package paintshop

import scala.annotation.tailrec
import scala.collection.mutable

object Mixer {

  def mix(selections: List[PaintSelection]): Option[PaintSelection] = {
    if (selections.isEmpty) None
    else {
      reduceSearchSpace(selections.toSet) match {
        case Some((mustHavePaints, reducedSelections)) =>
          if (reducedSelections.isEmpty) Some(PaintSelection(mustHavePaints))
          else {
            exploreSearchSpace(reducedSelections).map { mix =>
              PaintSelection(mustHavePaints ++ mix.paints)
            }
          }
          
        case None => None
      }
    }
  }

  private def reduceSearchSpace(selections: Set[PaintSelection]): Option[(Set[Paint], Set[PaintSelection])] = {
    implicit val ordering = Ordering.by[PaintSelection, Int](_.paints.size).reverse // small selections at the top
    var selectionQueue = mutable.PriorityQueue.empty[PaintSelection] ++ selections
    var singletonFound = true
    var deadEndFound = false
    val mustHavePaints = mutable.Set.empty[Paint]

    while (!selectionQueue.isEmpty && singletonFound && !deadEndFound) {
      val smallest = selectionQueue.dequeue()
      val paints = smallest.paints

      if (paints.isEmpty) {
        // throw away
      } else if (paints.size == 1) {
        val singleton = paints.head
        mustHavePaints += singleton
        val updated = mutable.PriorityQueue.empty[PaintSelection]

        // remove must-have paint from remaining selections
        while (!selectionQueue.isEmpty && !deadEndFound) {
          val ps = selectionQueue.dequeue().paints

          if (ps.size == 1 && ps.head.color == singleton.color && ps.head.sheen != singleton.sheen) {
            deadEndFound = true // incompatible with other singleton
          } else {
            updated += PaintSelection(ps.filterNot(_.color == singleton.color))
          }
        }
        selectionQueue = updated
      } else {
        selectionQueue += smallest
        singletonFound = false
      }
    }

    if (deadEndFound) None else Some(mustHavePaints.toSet -> selectionQueue.toSet)
  }

  private def exploreSearchSpace(selections: Set[PaintSelection]): Option[PaintSelection] = {
    def satisfiesAll(mix: Set[Paint]): Boolean = {
      def isHappy(asked: PaintSelection) = asked.paints.exists(mix.contains(_))

      selections.forall(isHappy)
    }

    val distinctColors = for ( s <- selections; p <- s.paints) yield p.color
    val solutions = findFeasibleSolutions(distinctColors, Sheen.all, satisfiesAll)
    solutions.sortBy(s => s.paints.foldRight(0)((p, totalCost) => totalCost + p.sheen.cost)).headOption
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
