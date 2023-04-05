package turtle

case class Turtle(
    position: Position = Position(),
    angle: Angle = Angle(),
    penState: PenState = PenState.Down,
    penColor: PenColor = PenColor.Black
)

enum PenState:
  case Up, Down

enum PenColor:
  case Red, Black

case class Position(x: Float = 0f, y: Float = 0f)
case class Angle(rotation: Float = 0f)
case class Distance(value: Float = 0f)

object Turtle:

  def move(turtle: Turtle, distance: Distance): Turtle =
    val newPos = calc_new_position(turtle.position, turtle.angle, distance)
    turtle.copy(position = newPos)

  def move2(turtle: Turtle, distance: Distance): (Turtle, Unit) =
    (move(turtle, distance), ())

  def turn(turtle: Turtle, angle: Angle): Turtle =
    turtle.copy(angle = angle)

  def turn2(turtle: Turtle, angle: Angle): (Turtle, Unit) =
    (turn(turtle, angle), ())

  def setPenColor(turtle: Turtle, penColor: PenColor): Turtle =
    turtle.copy(penColor = penColor)

  def setPenState(turtle: Turtle, penState: PenState): Turtle =
    turtle.copy(penState = penState)

  private def calc_new_position(
      curPosition: Position,
      curAngle: Angle,
      distance: Distance
  ): Position =
    val angle_in_rads = curAngle.rotation.toRadians
    val new_x =
      roundAt(2)(curPosition.x + distance.value * (math cos angle_in_rads))
    val new_y =
      roundAt(2)(curPosition.y + distance.value * (math sin angle_in_rads))

    Position(new_x.toFloat, new_y.toFloat)

  private def roundAt(p: Int)(n: Double): Double =
    val s = math pow (10, p);
    (math round n * s) / s
