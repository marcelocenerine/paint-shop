package paintshop

import org.scalatest.{EitherValues, FunSuite}
import paintshop.BatchOrder.ParseError

import scala.io.Source

class BatchOrderSuite extends FunSuite with EitherValues {

  test("should create empty") {
    assert(BatchOrder.empty.isEmpty)
  }

  test("should create empty from file") {
    assert(BatchOrder.from(file("0")).right.value.isEmpty)
  }

  test("should create from file") {
    val order = BatchOrder.from(file(
      """5
        |2 M
        |5 G
        |1 G
        |5 G 1 G 4 M
        |3 G
        |5 G
        |3 G 5 G 1 G
        |3 G
        |2 M
        |5 G 1 G
        |2 M
        |5 G
        |4 M
        |5 G 4 M""".stripMargin))

    assert(order.right.value.palette === Palette(
      Set(Color(1), Color(2), Color(3), Color(4), Color(5)),
      Set(Gloss, Matte))
    )
    assert(order.right.value.selections === List(
      PaintSelection(List(Paint(Color(2), Matte))),
      PaintSelection(List(Paint(Color(5), Gloss))),
      PaintSelection(List(Paint(Color(1), Gloss))),
      PaintSelection(List(Paint(Color(5), Gloss), Paint(Color(1), Gloss), Paint(Color(4), Matte))),
      PaintSelection(List(Paint(Color(3), Gloss))),
      PaintSelection(List(Paint(Color(5), Gloss))),
      PaintSelection(List(Paint(Color(3), Gloss), Paint(Color(5), Gloss), Paint(Color(1), Gloss))),
      PaintSelection(List(Paint(Color(3), Gloss))),
      PaintSelection(List(Paint(Color(2), Matte))),
      PaintSelection(List(Paint(Color(5), Gloss), Paint(Color(1), Gloss))),
      PaintSelection(List(Paint(Color(2), Matte))),
      PaintSelection(List(Paint(Color(5), Gloss))),
      PaintSelection(List(Paint(Color(4), Matte))),
      PaintSelection(List(Paint(Color(5), Gloss), Paint(Color(4), Matte)))
    ))
  }

  test("should fail to create from empty file") {
    val order = BatchOrder.from(file(""))

    assert(order.left.value === ParseError("Empty input file"))
  }

  test("should fail to create from file if color count is not a number") {
    val order = BatchOrder.from(file(
      """?
        |1 G""".stripMargin
    ))

    assert(order.left.value === ParseError("Invalid color count: '?'"))
  }

  test("should fail to create from file if color count is a negative number") {
    val order = BatchOrder.from(file(
      """-1
        |1 G""".stripMargin
    ))

    assert(order.left.value === ParseError("Invalid color count: '-1'"))
  }

  test("should fail to create from file if color count is zero but there are lines") {
    val order = BatchOrder.from(file(
      """0
        |1 G""".stripMargin
    ))

    assert(order.left.value === ParseError("Invalid color count: '0'"))
  }

  test("should fail to create from file if lines are missing") {
    val order = BatchOrder.from(file("5"))

    assert(order.left.value === ParseError("Color count informed but no lines"))
  }

  test("should fail to create from file if any line has invalid color") {
    val order = BatchOrder.from(file(
      """2
        |1 M
        |2 G
        |1 M 0 G""".stripMargin
    ))

    assert(order.left.value === ParseError("Invalid color '0' in line '1 M 0 G'"))
  }

  test("should fail to create from file if any line has color not in the palette") {
    val order = BatchOrder.from(file(
      """2
        |1 M
        |1 M 2 G 3 G""".stripMargin
    ))

    assert(order.left.value === ParseError("Invalid color '3' in line '1 M 2 G 3 G'"))
  }

  test("should fail to create from file if any line has invalid sheen") {
    val order = BatchOrder.from(file(
      """2
        |1 M
        |1 M 2 Z""".stripMargin
    ))

    assert(order.left.value === ParseError("Malformed line: '1 M 2 Z'"))
  }

  test("should fail to create from file if any line is not well formatted") {
    val order = BatchOrder.from(file(
      """2
        |1 M
        | 1 M  2, Z """.stripMargin
    ))

    assert(order.left.value === ParseError("Malformed line: ' 1 M  2, Z '"))
  }

  test("should fail to create from file if any line is empty") {
    val order = BatchOrder.from(file(
      """2
        |
        |1 M 2 G""".stripMargin
    ))

    assert(order.left.value === ParseError("Malformed line: ''"))
  }

  private def file(content: String): Source = Source.fromString(content)
}
