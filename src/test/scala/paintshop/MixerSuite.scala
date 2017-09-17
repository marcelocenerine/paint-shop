package paintshop

import org.scalatest.FunSuite

class MixerSuite extends FunSuite {

  test("should not find solution for empty selection input") {
    val mixedSelection = Mixer.mix(Nil, Palette(Set(Color(1)), Sheen.all))

    assert(mixedSelection === None)
  }

  test("should not find solution if palette does not contain all the necessary colors") {
    val selections = List(
      PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte))),
      PaintSelection(Set(Paint(Color(1), Gloss)))
    )
    val palette = Palette(Set(Color(1)), Set(Gloss, Matte))

    assert(Mixer.mix(selections, palette) === None)
  }

  test("should not find solution if palette does not contain all the necessary sheens") {
    val selections = List(
      PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte))),
      PaintSelection(Set(Paint(Color(1), Gloss)))
    )
    val palette = Palette(Set(Color(1), Color(2)), Set(Gloss))

    assert(Mixer.mix(selections, palette) === None)
  }

  test("should not find solution if more than one sheen for the same color are required") {
    val selections = List(
      PaintSelection(Set(Paint(Color(1), Gloss))),
      PaintSelection(Set(Paint(Color(1), Matte)))
    )
    val palette = Palette(Set(Color(1)), Sheen.all)

    assert(Mixer.mix(selections, palette) === None)
  }

  test("should find solution for mix 1") {
    val selections = List(
      PaintSelection(Set(Paint(Color(1), Gloss), Paint(Color(2), Matte))),
      PaintSelection(Set(Paint(Color(1), Matte)))
    )
    val palette = Palette(Set(Color(1), Color(2)), Sheen.all)

    val mixedSelection = Mixer.mix(selections, palette)

    assert(mixedSelection === Some(PaintSelection(Set(Paint(Color(1), Matte), Paint(Color(2), Matte)))))
  }

  test("should find solution for mix 2") {
    val selections = List(
      PaintSelection(Set(Paint(Color(1), Matte), Paint(Color(3), Gloss), Paint(Color(5), Gloss))),
      PaintSelection(Set(Paint(Color(2), Gloss), Paint(Color(3), Matte), Paint(Color(4), Gloss))),
      PaintSelection(Set(Paint(Color(5), Matte)))
    )
    val palette = Palette(Set(Color(1), Color(2), Color(3), Color(4), Color(5)), Sheen.all)

    val mixedSelection = Mixer.mix(selections, palette)

    assert(mixedSelection === Some(PaintSelection(Set(
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
    val palette = Palette(Set(Color(1), Color(2), Color(3), Color(4), Color(5)), Sheen.all)

    val mixedSelection = Mixer.mix(selections, palette)

    assert(mixedSelection === Some(PaintSelection(Set(
      Paint(Color(1), Gloss), Paint(Color(2), Matte), Paint(Color(3), Gloss), Paint(Color(4), Matte), Paint(Color(5), Gloss)))
    ))
  }
}
