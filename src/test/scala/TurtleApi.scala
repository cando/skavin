class TurtleApiTest extends munit.FunSuite {
  test("api move turtle success") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    val newT = TurtleApi.move(t, "12")
    assert(newT.isRight)
    assertEquals(newT.fold(identity, _.position), Position(12, 0))
  }

  test("api move turtle fail") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    val newT = TurtleApi.move(t, "NOOOOOOOOOOOO")
    assert(newT.isLeft)
    assertEquals(newT, Left(TurtleError.InvalidDistance))
  }

  test("api turn turtle success") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    val newT = TurtleApi.turn(t, "12")
    assert(newT.isRight)
    assertEquals(newT.fold(identity, _.angle), Angle(12))
  }

  test("api turn turtle fail") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    val newT = TurtleApi.turn(t, "NOOOOOOOOOOOO")
    assert(newT.isLeft)
    assertEquals(newT, Left(TurtleError.InvalidAngle))
  }

  test("api set pen color success") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    val newT = TurtleApi.setPenColor(t, "Black")
    assert(newT.isRight)
    assertEquals(newT.fold(identity, _.penColor), PenColor.Black)
  }

  test("api set pen color fail") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    val newT = TurtleApi.setPenColor(t, "NOOOO")
    assert(newT.isLeft)
    assertEquals(newT, Left(TurtleError.InvalidPenColor))
  }

  test("api set pen state success") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    val newT = TurtleApi.setPenState(t, "Down")
    assert(newT.isRight)
    assertEquals(newT.fold(identity, _.penState), PenState.Down)
  }

  test("api set pen state fail") {
    val t = Turtle(Position(0, 0), Angle(0), PenState.Up, PenColor.Red)
    val newT = TurtleApi.setPenState(t, "NOOOO")
    assert(newT.isLeft)
    assertEquals(newT, Left(TurtleError.InvalidPenState))
  }
}
