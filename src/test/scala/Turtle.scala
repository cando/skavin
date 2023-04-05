import turtle._

class TurtleTest extends munit.FunSuite {
  test("move turtle") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    val newT = Turtle.move(t, Distance(12))
    assertEquals(newT.position, Position(12, 0))
  }

  test("set turtle pen color") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    val newT = Turtle.setPenColor(t, PenColor.Black)
    assertEquals(newT.penColor, PenColor.Black)
  }

  test("set turtle pen state") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    val newT = Turtle.setPenState(t, PenState.Down)
    assertEquals(newT.penState, PenState.Down)
  }

  test("rotate turtle") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    var newT = Turtle.turn(t, Angle(90))
    assertEquals(newT.angle, Angle(90))

    newT = Turtle.move(newT, Distance(12))
    assertEquals(newT.position, Position(0, 12))
  }
}
