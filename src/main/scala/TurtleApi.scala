package turtle.api

import turtle._

import cats.syntax.all._

import scala.util.Try
import scala.util.Failure
import scala.util.Success
import cats.Applicative

enum TurtleError:
  case InvalidDistance, InvalidAngle, InvalidPenColor, InvalidPenState

object TurtleApi:
  def move(turtle: Turtle, distance: String): Either[TurtleError, Turtle] =
    validateDistance(distance).map(Turtle.move(turtle, _))

  def move2(
      turtle: Turtle,
      distance: String
  ): Either[TurtleError, (Turtle, Unit)] =
    validateDistance(distance).map(Turtle.move2(turtle, _))

  def turn(turtle: Turtle, angle: String): Either[TurtleError, Turtle] =
    validateAngle(angle).map(Turtle.turn(turtle, _))

  def turn2(
      turtle: Turtle,
      angle: String
  ): Either[TurtleError, (Turtle, Unit)] =
    validateAngle(angle).map(Turtle.turn2(turtle, _))

  def setPenColor(
      turtle: Turtle,
      color: String
  ): Either[TurtleError, Turtle] =
    validatePenColor(color).map(Turtle.setPenColor(turtle, _))

  def setPenState(
      turtle: Turtle,
      state: String
  ): Either[TurtleError, Turtle] =
    validatePenState(state).map(Turtle.setPenState(turtle, _))

  def validateDistance(
      distance: String
  ): Either[TurtleError, Distance] =
    Try(distance.toFloat).toEither
      .bimap(_ => TurtleError.InvalidDistance, Distance(_))

  def validateAngle(
      angle: String
  ): Either[TurtleError, Angle] =
    Try(angle.toFloat).toEither
      .bimap(_ => TurtleError.InvalidAngle, Angle(_))

  def validatePenColor(
      color: String
  ): Either[TurtleError, PenColor] =
    Try(PenColor.valueOf(color)).toEither
      .bimap(_ => TurtleError.InvalidPenColor, identity)

  def validatePenState(
      color: String
  ): Either[TurtleError, PenState] =
    Try(PenState.valueOf(color)).toEither
      .bimap(_ => TurtleError.InvalidPenState, identity)
