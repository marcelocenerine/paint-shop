package paintshop

import org.scalatest.FunSuite
import scala.concurrent.duration._
import scala.language.postfixOps

class MixerSuite extends FunSuite {

  List(BruteForceMixer, new TabuSearchMixer(2 seconds)).foreach { mixer =>
    val name = mixer.getClass.getSimpleName

    test(s"$name - should not find solution for empty input") {
      val mixedSelection = mixer.mix(Nil)

      assert(mixedSelection === None)
    }

    test(s"$name - should find solution for empty selection") {
      val mixedSelection = mixer.mix(List(PaintSelection(Set.empty)))

      assert(mixedSelection === Some(PaintSelection(Set.empty)))
    }

    test(s"$name - should not find solution if more than one sheen for the same color are required") {
      val selections = List(
        PaintSelection(Set(Paint(Color(1), Gloss))),
        PaintSelection(Set(Paint(Color(1), Matte)))
      )

      assert(mixer.mix(selections) === None)
    }

    test(s"$name - should find solution for equal paints") {
      val selections = List(
        PaintSelection(Set(Paint(Color(1), Gloss))),
        PaintSelection(Set(Paint(Color(1), Gloss)))
      )

      assert(mixer.mix(selections) === Some(PaintSelection(Set(Paint(Color(1), Gloss)))))
    }

    test(s"$name - should find solution for different paints") {
      val selections = List(
        PaintSelection(Set(Paint(Color(1), Gloss))),
        PaintSelection(Set(Paint(Color(2), Matte)))
      )

      assert(mixer.mix(selections) === Some(PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte)))))
    }

    test(s"$name - should find solution when fixed paint does not match sheen in other selections") {
      val selections = List(
        PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte))),
        PaintSelection(Set(Paint(Color(1), Matte)))
      )

      assert(mixer.mix(selections) === Some(PaintSelection(Set(Paint(Color(1), Matte), Paint(Color(2), Matte)))))
    }

    test(s"$name - should find solution when fixed paint matches sheen in other selections") {
      val selections = List(
        PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte))),
        PaintSelection(Set(Paint(Color(2), Matte)))
      )

      assert(mixer.mix(selections) === Some(PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte)))))
    }

    test(s"$name - should find solution when fixed paint matches sheen in some selection") {
      val selections = List(
        PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte))),
        PaintSelection(Set(Paint(Color(1), Matte), Paint(Color(2), Gloss))),
        PaintSelection(Set(Paint(Color(2), Matte)))
      )

      assert(mixer.mix(selections) === Some(PaintSelection(Set(Paint(Color(1), Matte), Paint(Color(2), Matte)))))
    }

    test(s"$name - should find solution when fixed paint matches sheen in some other selection with unique paint") {
      val selections = List(
        PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte), Paint(Color(3), Gloss))),
        PaintSelection(Set(Paint(Color(1), Matte), Paint(Color(2), Gloss))),
        PaintSelection(Set(Paint(Color(2), Matte)))
      )

      assert(mixer.mix(selections) === Some(PaintSelection(Set(Paint(Color(1), Matte), Paint(Color(2), Matte), Paint(Color(3), Gloss)))))
    }

    test(s"$name - should find solution for singleton selections") {
      val selections = List(
        PaintSelection(Set(Paint(Color(1), Gloss))),
        PaintSelection(Set(Paint(Color(2), Matte))),
        PaintSelection(Set(Paint(Color(3), Gloss))),
        PaintSelection(Set(Paint(Color(4), Matte))),
        PaintSelection(Set(Paint(Color(5), Gloss)))
      )

      assert(mixer.mix(selections) === Some(PaintSelection(Set(
        Paint(Color(1), Gloss), Paint(Color(2), Matte), Paint(Color(3), Gloss), Paint(Color(4), Matte), Paint(Color(5), Gloss)))
      ))
    }

    test(s"$name - should find solution for selections with increasing number of paints") {
      val selections = List(
        PaintSelection(Set(Paint(Color(1), Gloss))),
        PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte))),
        PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte), Paint(Color(3), Gloss))),
        PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte), Paint(Color(3), Gloss), Paint(Color(4), Matte))),
        PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte), Paint(Color(3), Gloss), Paint(Color(4), Matte), Paint(Color(5), Gloss)))
      )

      assert(mixer.mix(selections) === Some(PaintSelection(Set(
        Paint(Color(1), Gloss), Paint(Color(2), Gloss), Paint(Color(3), Gloss), Paint(Color(4), Gloss), Paint(Color(5), Gloss)))
      ))
    }

    test(s"$name - should find solution for sample input 1") {
      val selections = List(
        PaintSelection(Set(Paint(Color(1), Matte), Paint(Color(3), Gloss), Paint(Color(5), Gloss))),
        PaintSelection(Set(Paint(Color(2), Gloss), Paint(Color(3), Matte), Paint(Color(4), Gloss))),
        PaintSelection(Set(Paint(Color(5), Matte)))
      )

      assert(mixer.mix(selections) === Some(PaintSelection(Set(
        Paint(Color(1), Gloss), Paint(Color(2), Gloss), Paint(Color(3), Gloss), Paint(Color(4), Gloss), Paint(Color(5), Matte)))
      ))
    }

    test(s"$name - should find solution for sample input 2") {
      val selections = List(
        PaintSelection(Set(Paint(Color(2), Matte))),
        PaintSelection(Set(Paint(Color(5), Gloss))),
        PaintSelection(Set(Paint(Color(1), Gloss))),
        PaintSelection(Set(Paint(Color(5), Gloss), Paint(Color(1), Gloss), Paint(Color(4), Matte))),
        PaintSelection(Set(Paint(Color(3), Gloss))),
        PaintSelection(Set(Paint(Color(5), Gloss))),
        PaintSelection(Set(Paint(Color(3), Gloss), Paint(Color(5), Gloss), Paint(Color(1), Gloss))),
        PaintSelection(Set(Paint(Color(3), Gloss))),
        PaintSelection(Set(Paint(Color(2), Matte))),
        PaintSelection(Set(Paint(Color(5), Gloss), Paint(Color(1), Gloss))),
        PaintSelection(Set(Paint(Color(2), Matte))),
        PaintSelection(Set(Paint(Color(5), Gloss))),
        PaintSelection(Set(Paint(Color(4), Matte))),
        PaintSelection(Set(Paint(Color(5), Gloss), Paint(Color(4), Matte)))
      )

      assert(mixer.mix(selections) === Some(PaintSelection(Set(
        Paint(Color(1), Gloss), Paint(Color(2), Matte), Paint(Color(3), Gloss), Paint(Color(4), Matte), Paint(Color(5), Gloss)))
      ))
    }

    test(s"$name - should find solution for sample input 3") {
      val selections = List(
        PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte), Paint(Color(3), Gloss), Paint(Color(4), Matte), Paint(Color(5), Gloss))),
        PaintSelection(Set(Paint(Color(1), Matte), Paint(Color(2), Gloss), Paint(Color(3), Matte), Paint(Color(4), Gloss), Paint(Color(5), Matte))),
        PaintSelection(Set(Paint(Color(2), Matte), Paint(Color(3), Gloss), Paint(Color(4), Matte), Paint(Color(5), Gloss), Paint(Color(6), Matte))),
        PaintSelection(Set(Paint(Color(3), Matte), Paint(Color(4), Gloss), Paint(Color(5), Matte), Paint(Color(6), Gloss), Paint(Color(7), Matte)))
      )

      assert(mixer.mix(selections) === Some(PaintSelection(Set(
        Paint(Color(1), Gloss), Paint(Color(2), Gloss), Paint(Color(3), Gloss), Paint(Color(4), Gloss),
        Paint(Color(5), Gloss), Paint(Color(6), Gloss), Paint(Color(7), Gloss)))
      ))
    }
  }
}
