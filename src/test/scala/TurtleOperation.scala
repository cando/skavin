import cats.data.StateT
import cats.data.EitherT
import cats.data.Writer
import cats.kernel.Monoid

class TurtleOperation extends munit.FunSuite {
  test("simple turtle") {
    val t = Turtle()
    var newT = Turtle.turn(t, Angle(90))
    newT = Turtle.move(newT, Distance(12))
    newT = Turtle.setPenColor(newT, PenColor.Red)
    newT = Turtle.setPenState(newT, PenState.Down)

    assertEquals(
      newT,
      Turtle(
        position = Position(0, 12),
        angle = Angle(90),
        penColor = PenColor.Red,
        penState = PenState.Down
      )
    )

  }

  test("chaining api turtle success") {
    val t = Turtle()
    var newT = TurtleApi
      .turn(t, "90")
      .flatMap(
        TurtleApi
          .move(_, "12")
          .flatMap(
            TurtleApi
              .setPenColor(_, "Red")
              .flatMap(TurtleApi.setPenState(_, "Down"))
          )
      )

    assertEquals(
      newT.fold(identity, identity),
      Turtle(
        position = Position(0, 12),
        angle = Angle(90),
        penColor = PenColor.Red,
        penState = PenState.Down
      )
    )

  }

  test("chaining api turtle fail") {
    val t = Turtle()
    var newT = TurtleApi
      .turn(t, "90")
      .flatMap(
        TurtleApi
          .move(_, "WRONGGGGGG")
          .flatMap(
            TurtleApi
              .setPenColor(_, "Red")
              .flatMap(TurtleApi.setPenState(_, "Down"))
          )
      )

    assertEquals(
      newT,
      Left(TurtleError.InvalidDistance)
    )

  }

  test("for notation api turtle success") {
    val turtle = Turtle()
    var newT = for {
      t <- TurtleApi.turn(turtle, "90")
      t <- TurtleApi.move(t, "12")
      t <- TurtleApi.setPenColor(t, "Red")
      t <- TurtleApi.setPenState(t, "Down")
    } yield t

    assertEquals(
      newT.fold(identity, identity),
      Turtle(
        position = Position(0, 12),
        angle = Angle(90),
        penColor = PenColor.Red,
        penState = PenState.Down
      )
    )

  }

  test("for notation api turtle fail") {
    val turtle = Turtle()
    var newT = for {
      t <- TurtleApi.turn(turtle, "90")
      t <- TurtleApi.move(t, "12")
      t <- TurtleApi.setPenColor(t, "NOOOOOOOOOOO")
      t <- TurtleApi.setPenState(t, "Down")
    } yield t

    assertEquals(
      newT,
      Left(TurtleError.InvalidPenColor)
    )

  }

  test("state monad simple turtle success") {
    import cats.data.State

    val doTurn: Angle => State[Turtle, Unit] =
      (angle: Angle) => State(t => (Turtle.turn(t, angle), ()))
    val doMove: Distance => State[Turtle, Unit] =
      (distance: Distance) => State(t => (Turtle.move(t, distance), ()))
    val doSetPenColor: PenColor => State[Turtle, Unit] =
      (color: PenColor) => State(t => (Turtle.setPenColor(t, color), ()))
    val doSetPenState: PenState => State[Turtle, Unit] =
      (state: PenState) => State(t => (Turtle.setPenState(t, state), ()))

    var computation = for {
      _ <- doTurn(Angle(90))
      _ <- doMove(Distance(12))
      _ <- doSetPenColor(PenColor.Red)
      _ <- doSetPenState(PenState.Down)
    } yield ()

    val newT = computation.runS(Turtle()).value

    assertEquals(
      newT,
      Turtle(
        position = Position(0, 12),
        angle = Angle(90),
        penColor = PenColor.Red,
        penState = PenState.Down
      )
    )

  }

  test("state monad transformer api turtle success") {
    import cats.data.StateT

    type EitherP[A] = Either[TurtleError, A]

    val doTurn: String => StateT[EitherP, Turtle, Unit] =
      (angle: String) => StateT(t => TurtleApi.turn2(t, angle))
    val doMove: String => StateT[EitherP, Turtle, Unit] =
      (distance: String) => StateT(t => TurtleApi.move2(t, distance))

    var computation = for {
      _ <- doTurn("90")
      _ <- doMove("12")
    } yield ()

    val newT = computation.run(Turtle())

    assertEquals(
      newT.fold(identity, _._1),
      Turtle(
        position = Position(0, 12),
        angle = Angle(90),
        penColor = PenColor.Black,
        penState = PenState.Down
      )
    )

  }

  test("state monad transformer api turtle fail") {
    import cats.data.StateT

    type EitherTurtleError[A] = Either[TurtleError, A]

    val doTurn: String => StateT[EitherTurtleError, Turtle, Unit] =
      (angle: String) => StateT(t => TurtleApi.turn2(t, angle))
    val doMove: String => StateT[EitherTurtleError, Turtle, Unit] =
      (distance: String) => StateT(t => TurtleApi.move2(t, distance))

    var computation = for {
      _ <- doTurn("90")
      _ <- doMove("NAAAAAAAAAAA")
    } yield ()

    val newT = computation.run(Turtle())

    assertEquals(
      newT,
      Left(TurtleError.InvalidDistance)
    )

  }

  test("state and writer and either monad transformer api turtle success") {
    val doTurn: String => StateT[EitherTurtleError, Turtle, Unit] =
      (angle) =>
        StateT(t =>
          EitherT(Writer(s"turning $angle\n", TurtleApi.turn2(t, angle)))
        )

    val doMove: String => StateT[EitherTurtleError, Turtle, Unit] =
      (distance) =>
        StateT(t =>
          EitherT(Writer(s"moving $distance\n", TurtleApi.move2(t, distance)))
        )

    var computation = for {
      _ <- doTurn("90")
      _ <- doMove("12")
    } yield ()

    val newT = computation.run(Turtle()).value.run

    assertEquals(
      newT._1,
      "turning 90\nmoving 12\n"
    )

    assertEquals(
      newT._2.fold(identity, _._1),
      Turtle(
        position = Position(0, 12),
        angle = Angle(90),
        penColor = PenColor.Black,
        penState = PenState.Down
      )
    )

  }

  test("state and writer and either monad transformer lifting api success") {

    val validateDistance = liftValidation(TurtleApi.validateDistance)
    val validateAngle = liftValidation(TurtleApi.validateAngle)

    val doTurn = liftOp(Turtle.turn2)
    val doMove = liftOp(Turtle.move2)

    var computation = for {
      _ <- log("turning 90\n")
      d <- validateAngle("90")
      _ <- doTurn(d)

      _ <- log("moving 12\n")
      d <- validateDistance("12")
      _ <- doMove(d)
    } yield ()

    val newT = computation.run(Turtle()).value.run

    assertEquals(
      newT._1,
      "turning 90\nmoving 12\n"
    )

    assertEquals(
      newT._2.fold(identity, _._1),
      Turtle(
        position = Position(0, 12),
        angle = Angle(90),
        penColor = PenColor.Black,
        penState = PenState.Down
      )
    )

  }

  test("final solution") {

    var computation: TurtleM[_] = for {
      _ <- turnTurtle("90")
      _ <- moveTurtle("12")
    } yield ()

    val newT = computation.run(Turtle()).value.run

    assertEquals(
      newT._1,
      "turning 90\nmoving 12\n"
    )

    assertEquals(
      newT._2.fold(identity, _._1),
      Turtle(
        position = Position(0, 12),
        angle = Angle(90),
        penColor = PenColor.Black,
        penState = PenState.Down
      )
    )

  }

  type WriterTurtle[V] = Writer[String, V]
  type EitherTurtleError[A] = EitherT[WriterTurtle, TurtleError, A]
  type TurtleM[A] = StateT[EitherTurtleError, Turtle, A]

  def log(
      l: String
  ): TurtleM[Unit] =
    StateT.liftF(
      EitherT.liftF(Writer.tell(l))
    )

  def liftValidation[A](
      validationFun: String => Either[TurtleError, A]
  ): String => TurtleM[A] =
    p =>
      StateT.liftF(
        EitherT(Writer.value(validationFun(p)))
      )

  def liftOp[A](
      opFun: (Turtle, A) => (Turtle, Unit)
  ): A => TurtleM[Unit] =
    p => StateT(t => EitherT.liftF(Writer.value(opFun(t, p))))

  def turnTurtle(angle: String) =
    val validateAngle = liftValidation(TurtleApi.validateAngle)
    val doTurn = liftOp(Turtle.turn2)

    for {
      _ <- log(s"turning $angle\n")
      d <- validateAngle(angle)
      _ <- doTurn(d)

    } yield ()

  def moveTurtle(distance: String) =
    val validateDistance = liftValidation(TurtleApi.validateDistance)
    val doMove = liftOp(Turtle.move2)

    for {
      _ <- log(s"moving $distance\n")
      d <- validateDistance(distance)
      _ <- doMove(d)

    } yield ()

}
