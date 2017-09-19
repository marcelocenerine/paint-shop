package paintshop

import java.time.Clock

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.Random

sealed trait Mixer {

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

  protected def exploreSearchSpace(selections: Set[PaintSelection]): Option[PaintSelection]

  private def reduceSearchSpace(selections: Set[PaintSelection]): Option[(Set[Paint], Set[PaintSelection])] = {
    implicit val ordering = Ordering.by[PaintSelection, Int](_.paints.size).reverse // small selections at the top
    var selectionQueue = mutable.PriorityQueue.empty[PaintSelection] ++ selections
    var singletonFound = true
    var deadEndFound = false
    val mustHavePaints = mutable.Set.empty[Paint]
    def sameColorDifferentSheen(a: Paint, b: Paint) = a.color == b.color && a.sheen != b.sheen
    def isSingleton(xs: Set[_]) = xs.size == 1

    while (selectionQueue.nonEmpty && singletonFound && !deadEndFound) {
      val smallest = selectionQueue.dequeue()
      val paints = smallest.paints

      if (paints.isEmpty) {
        // throw away
      } else if (isSingleton(paints)) {
        val singleton = paints.head
        mustHavePaints += singleton
        val updatedQueue = mutable.PriorityQueue.empty[PaintSelection]

        // remove must-have paint from remaining selections
        while (selectionQueue.nonEmpty && !deadEndFound) {
          val ps = selectionQueue.dequeue().paints

          if (isSingleton(ps) && sameColorDifferentSheen(ps.head, singleton)) {
            deadEndFound = true // incompatible with other singleton
          } else {
            updatedQueue += PaintSelection(ps.filterNot(_.color == singleton.color))
          }
        }
        selectionQueue = updatedQueue
      } else {
        selectionQueue += smallest
        singletonFound = false
      }
    }

    if (deadEndFound) None else Some(mustHavePaints.toSet -> selectionQueue.toSet)
  }

  protected def satisfiesAll(selections: Set[PaintSelection])(mix: Set[Paint]): Boolean = {
    def isHappy(asked: PaintSelection) = asked.paints.exists(p => mix.contains(p))

    selections.forall(isHappy)
  }

  protected def cost(mix: Set[Paint]): Int = mix.foldRight(0)((p, totalCost) => totalCost + p.sheen.cost)
}


/**
  * Explores the search space using an exhaustive search (brute-force) algorithm. The time complexity of this algorithm
  * is `O(n^m)` - exponential - in the worst case scenario, where n = sheens and m = colors. This characteristic
  * makes it not scalable to large inputs.
  */
object BruteForceMixer extends Mixer {

  protected def exploreSearchSpace(selections: Set[PaintSelection]): Option[PaintSelection] = {
    val distinctColors = for ( s <- selections; p <- s.paints) yield p.color
    val feasibleSolutions = findFeasibleSolutions(distinctColors, Sheen.all, satisfiesAll(selections))
    feasibleSolutions.sortBy(cost).headOption.map(PaintSelection)
  }

  private def findFeasibleSolutions(colors: Set[Color], sheens: Set[Sheen], p: Set[Paint] => Boolean): List[Set[Paint]] = {
    @tailrec
    def combs(currentColors: List[Color], partialCombs: List[Set[Paint]]): List[Set[Paint]] = {
      currentColors match {
        case head :: tail =>
          val newPartialCombs = for {
            comb <- partialCombs
            sheen <- sheens
          } yield comb + Paint(head, sheen)
          combs(tail, newPartialCombs)

        case Nil => partialCombs.filter(p)
      }
    }

    combs(colors.toList, List(Set.empty))
  }
}


/**
  * Explores the search space using the Tabu Search meta-heuristic: https://en.wikipedia.org/wiki/Tabu_search
  *
  * This algorithm is suitable for large inputs where the search space is just too big to be explored via exhaustive
  * search. Given that the algorithm may not visit all possible combinations in the search space, it's not guaranteed
  * that it will find an optimal nor a feasible solution. The more time is given to the algorithm, the better the
  * solution tends to be.
  *
  * @param localSearchDuration - max amount of time to be spent in the local search
  * @param clock - system clock
  */
class TabuSearchMixer(localSearchDuration: Duration, clock: Clock = Clock.systemDefaultZone()) extends Mixer {

  private val TabuSize = 1000
  private val random = new Random()

  protected def exploreSearchSpace(selections: Set[PaintSelection]): Option[PaintSelection] = {
    val distinctColors = for ( s <- selections; p <- s.paints ) yield p.color
    val deadline = clock.millis() + localSearchDuration.toMillis

    def scoreCalculator(mix: Set[Paint]): Int = if (satisfiesAll(selections)(mix)) -cost(mix) else Int.MinValue
    def stopCondition: Boolean = clock.millis() > deadline

    val bestSolutionFound = search(distinctColors, Sheen.all, scoreCalculator, stopCondition)
    if (satisfiesAll(selections)(bestSolutionFound)) Some(PaintSelection(bestSolutionFound)) else None
  }

  private def search(colors: Set[Color], sheens: Set[Sheen], scoreCalc: Set[Paint] => Int, stopCond: => Boolean): Set[Paint] = {
    var currentSolution: Array[Paint] = initialSolution(colors, sheens)
    var bestSolution = currentSolution
    var bestSolutionScore: Int = scoreCalc(bestSolution.toSet)
    val tabuList = new TabuList(TabuSize)
    tabuList.add(bestSolution.toSet)

    while (!stopCond) {
      currentSolution = randomNeighbor(currentSolution, sheens)
      val candidate = currentSolution.toSet

      if (!tabuList.contains(candidate)) {
        val score = scoreCalc(candidate)

        if (score > bestSolutionScore) {
          bestSolution = currentSolution
          bestSolutionScore = score
        }

        tabuList.add(candidate)
      }
    }

    bestSolution.toSet
  }

  private def initialSolution(colors: Set[Color], sheens: Set[Sheen]): Array[Paint] = {
    val cheapestSheen = sheens.min
    colors.map(color => Paint(color, cheapestSheen)).toArray
  }

  private def randomNeighbor(sol: Array[Paint], sheens: Set[Sheen]): Array[Paint] = {
    val neighbor = sol.clone()
    val nr = random.nextInt(neighbor.length)
    val oldPaint = sol(nr)
    val diffSheens = (sheens - oldPaint.sheen).toArray
    val sr = random.nextInt(diffSheens.length)
    neighbor(nr) = Paint(oldPaint.color, diffSheens(sr))
    neighbor
  }

  private class TabuList(size: Int) {

    private val solutionTabuQueue = mutable.Queue.fill[Any](size)(null)
    private val solutionTabuSet = mutable.Set.empty[Any] // for fast search

    def contains(s: Any): Boolean = solutionTabuSet.contains(s)

    def add(s: Any): Unit = {
      val removed = solutionTabuQueue.dequeue()
      solutionTabuSet - removed
      solutionTabuQueue += s
      solutionTabuSet += s
    }
  }
}
