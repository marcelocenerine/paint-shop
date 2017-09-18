package paintshop

import org.scalatest.FunSuite

class MixerSuite extends FunSuite {

  test("should not find solution for empty input") {
    val mixedSelection = Mixer.mix(Nil)

    assert(mixedSelection === None)
  }

  test("should not find solution if more than one sheen for the same color are required") {
    val selections = List(
      PaintSelection(Set(Paint(Color(1), Gloss))),
      PaintSelection(Set(Paint(Color(1), Matte)))
    )

    assert(Mixer.mix(selections) === None)
  }

  test("should find solution for empty selection") {
    val mixedSelection = Mixer.mix(List(PaintSelection(Set.empty)))

    assert(mixedSelection === Some(PaintSelection(Set.empty)))
  }

  test("should find solution for mix 1") {
    val selections = List(
      PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte))),
      PaintSelection(Set(Paint(Color(1), Matte)))
    )

    assert(Mixer.mix(selections) === Some(PaintSelection(Set(Paint(Color(1), Matte), Paint(Color(2), Matte)))))
  }

  test("should find solution for mix 2") {
    val selections = List(
      PaintSelection(Set(Paint(Color(1), Matte), Paint(Color(3), Gloss), Paint(Color(5), Gloss))),
      PaintSelection(Set(Paint(Color(2), Gloss), Paint(Color(3), Matte), Paint(Color(4), Gloss))),
      PaintSelection(Set(Paint(Color(5), Matte)))
    )

    assert(Mixer.mix(selections) === Some(PaintSelection(Set(
        Paint(Color(1), Gloss), Paint(Color(2), Gloss), Paint(Color(3), Gloss), Paint(Color(4), Gloss), Paint(Color(5), Matte)))
    ))
  }

  test("should find solution for mix 3") {
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

    assert(Mixer.mix(selections) === Some(PaintSelection(Set(
      Paint(Color(1), Gloss), Paint(Color(2), Matte), Paint(Color(3), Gloss), Paint(Color(4), Matte), Paint(Color(5), Gloss)))
    ))
  }
}
