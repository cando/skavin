package turtle.free

import turtle._
import turtle.api._

import cats.free.Free
import cats.free.Free.liftF
import cats.~>
import cats.data.{StateT, State}
import cats.Id

enum TurtleOpA[A]:
  case Move(distance: String) extends TurtleOpA[Unit]
  case Turn(angle: String) extends TurtleOpA[Unit]
  case Done() extends TurtleOpA[Turtle]

type TurtleOp[A] = Free[TurtleOpA, A]

// API
def move(distance: String): TurtleOp[Unit] =
  liftF[TurtleOpA, Unit](TurtleOpA.Move(distance))

def turn(angle: String): TurtleOp[Unit] =
  liftF[TurtleOpA, Unit](TurtleOpA.Turn(angle))

def done(): TurtleOp[Turtle] =
  liftF[TurtleOpA, Turtle](TurtleOpA.Done())

// INTERPRETERS
type EitherTurtleError[A] = Either[TurtleError, A]

def apiHackyVarCompiler: TurtleOpA ~> EitherTurtleError =
  new (TurtleOpA ~> EitherTurtleError) {
    var turtle = Turtle()

    def apply[A](fa: TurtleOpA[A]): EitherTurtleError[A] =
      fa match {
        case TurtleOpA.Move(distance) =>
          val res = TurtleApi.move(turtle, distance)
          turtle = res.getOrElse(turtle)
          Right(())

        case TurtleOpA.Turn(angle) =>
          val res = TurtleApi.turn(turtle, angle)
          turtle = res.getOrElse(turtle)
          Right(())

        case TurtleOpA.Done() =>
          Right(turtle)
      }
  }

type TurtleM[A] = StateT[EitherTurtleError, Turtle, A]

def apiStateCompiler: TurtleOpA ~> TurtleM =
  new (TurtleOpA ~> TurtleM) {
    def apply[A](fa: TurtleOpA[A]): TurtleM[A] =
      fa match {
        case TurtleOpA.Move(distance) =>
          StateT.modifyF(TurtleApi.move(_, distance))

        case TurtleOpA.Turn(angle) =>
          StateT.modifyF(TurtleApi.turn(_, angle))

        case TurtleOpA.Done() =>
          StateT.inspect(identity)

      }
  }
