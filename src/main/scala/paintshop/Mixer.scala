package paintshop

import java.time.Clock

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.Random

sealed trait Mixer {

  protected type Mix = Set[Paint]
  protected type IndexedMix = Array[Paint]

  protected val sheens: Set[Sheen] = Sheen.all
  protected lazy val cheapestSheen: Sheen = sheens.min

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

  /**
    * Reduces the search space by eliminating singleton selections which can only be satisfied by a unique value.
    * This reduction is performed recursively so that other multi-value selections can become singleton during the
    * reduction process. Unfeasible combinations derived from singleton selections are also identified at this stage,
    * which prevents pointless execution of subsequent phases.
    */
  private def reduceSearchSpace(selections: Set[PaintSelection]): Option[(Mix, Set[PaintSelection])] = {
    val fixedPaints = mutable.Set.empty[Paint]
    val fixedColors = mutable.Set.empty[Color]
    val colorToSelCount = mutable.Map.empty ++ groupedByColor(selections).mapValues(_.size)
    var reducedSelections = mutable.Set.empty ++ selections
    var singletonFound = true
    var conflictFound = false

    while (reducedSelections.nonEmpty && singletonFound && !conflictFound) {
      singletonFound = false
      val visited = mutable.Set.empty[PaintSelection]

      for (selection <- reducedSelections) {
        val paints = selection.paints

        if (paints.size == 1) {
          val singleton = paints.head

          if (fixedColors.contains(singleton.color)) {
            if (!fixedPaints.contains(singleton)) conflictFound = true // incompatible with other singleton
            else colorToSelCount(singleton.color) -= 1
          } else {
            fixedPaints += singleton
            fixedColors += singleton.color
            colorToSelCount(singleton.color) -= 1
          }
          singletonFound = true
        } else if (paints.nonEmpty) {
          val isSatisfied = paints.exists(fixedPaints.contains)

          if (isSatisfied) {
            for (p <- paints) {
              if (colorToSelCount(p.color) == 1 && !fixedColors.contains(p.color)) {
                fixedPaints += Paint(p.color, cheapestSheen) // not found in any other selection. Can be the cheapest
                fixedColors += p.color
              }
              colorToSelCount(p.color) -= 1
            }
          } else {
            val (fixed, nonFixed) = paints.partition(p => fixedColors.contains(p.color))

            if (fixed.nonEmpty) {
              fixed.foreach(p => colorToSelCount(p.color) -= 1)
              if (nonFixed.nonEmpty) visited += PaintSelection(nonFixed)
            } else {
              visited += selection // not satisfied nor could be reduced. Put it back
            }
          }
        }
      }
      reducedSelections = visited
    }

    if (conflictFound) None else Some(fixedPaints.toSet, reducedSelections.toSet)
  }

  protected def allColors(selections: Set[PaintSelection]): Set[Color] =
    for (s <- selections; p <- s.paints) yield p.color

  private def groupedByColor(selections: Set[PaintSelection]): Map[Color, Set[PaintSelection]] =
    selections.flatMap(sel => sel.paints.map(paint => (paint.color, sel)))
      .groupBy { case (color, _) => color }
      .mapValues(grouped => grouped.map { case (_, sel) => sel })

  protected def satisfiesAll(selections: Set[PaintSelection])(mix: Mix): Boolean = {
    def isHappy(asked: PaintSelection) = asked.paints.exists(p => mix.contains(p))
    selections.forall(isHappy)
  }

  protected def cost(mix: Mix): Int = mix.foldRight(0)((p, totalCost) => totalCost + p.sheen.cost)
}


/**
  * Explores the search space using an exhaustive search (brute-force) algorithm. The time complexity of this algorithm
  * is `O(n^m)` - exponential - in the worst case scenario, where n = sheens and m = colors. This characteristic
  * makes it not scalable to large inputs.
  */
object BruteForceMixer extends Mixer {

  protected def exploreSearchSpace(selections: Set[PaintSelection]): Option[PaintSelection] = {
    val distinctColors = allColors(selections)
    val feasibleSolutions = findFeasibleSolutions(distinctColors, satisfiesAll(selections))
    feasibleSolutions.sortBy(cost).headOption.map(PaintSelection)
  }

  private def findFeasibleSolutions(colors: Set[Color], p: Mix => Boolean): List[Mix] = {
    @tailrec
    def combs(currentColors: List[Color], partialCombs: List[Mix]): List[Mix] = {
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
    val distinctColors = allColors(selections)
    val deadline = clock.millis() + localSearchDuration.toMillis

    def scoreCalculator(mix: Mix): Int = if (satisfiesAll(selections)(mix)) -cost(mix) else Int.MinValue
    def stopCondition: Boolean = clock.millis() > deadline

    val bestSolutionFound = search(distinctColors, scoreCalculator, stopCondition)
    if (satisfiesAll(selections)(bestSolutionFound)) Some(PaintSelection(bestSolutionFound)) else None
  }

  private def search(colors: Set[Color], scoreCalc: Mix => Int, stopCond: => Boolean): Mix = {
    var currentSolution: IndexedMix = initialSolution(colors)
    var bestSolution = currentSolution
    var bestSolutionScore: Int = scoreCalc(bestSolution.toSet)
    val tabuList = new TabuList(TabuSize)
    tabuList.add(bestSolution.toSet)

    while (!stopCond) {
      val neighbors = randomNeighbors(currentSolution)
      val (bestCandidate, candidateScore) = pickBestCandidate(neighbors, tabuList, scoreCalc)

      if (candidateScore > bestSolutionScore) {
        bestSolution = bestCandidate
        bestSolutionScore = candidateScore
      }

      tabuList.add(bestCandidate.toSet)
      currentSolution = bestCandidate
    }

    bestSolution.toSet
  }

  private def initialSolution(colors: Set[Color]): IndexedMix = colors.map(Paint(_, cheapestSheen)).toArray

  private def randomNeighbors(originalSelection: IndexedMix): Seq[IndexedMix] = {
    val moveIndex = random.nextInt(originalSelection.length)
    val oldPaint = originalSelection(moveIndex) // chooses one paint randomly

    (sheens - oldPaint.sheen).toSeq.map { diffSheen =>
      val neighbor = originalSelection.clone()
      neighbor(moveIndex) = Paint(oldPaint.color, diffSheen) // sets a different sheen
      neighbor
    }
  }

  private def pickBestCandidate(candidates: Seq[IndexedMix], tabuList: TabuList, scoreCalc: Mix => Int): (IndexedMix, Int) = {
    var best = candidates.head
    var bestScore = scoreCalc(best.toSet)

    for (candidate <- candidates.tail) {
      val asSet = candidate.toSet

      if (!tabuList.contains(asSet)) {
        val score = scoreCalc(asSet)

        if (score > bestScore) {
          best = candidate
          bestScore = score
        }
      }
    }
    (best, bestScore)
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
