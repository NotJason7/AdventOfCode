package com.jbullock.aoc2020.day12

@main def solvePuzzle(): Unit =
  val input         = scala.io.Source.fromResource("aoc/2020/Day12/Input.txt").getLines.toVector
  val actions       = input.flatMap(Action.fromString)
  val waypointStart = Waypoint(10, -1)
  val start         = State(0, 0, East, waypointStart)
  val part1 = actions.foldLeft(start)((s: State, a: Action) => s.takeActionDirect(a)).manhattanDistanceFromOrigin
  println(s"Part 1: $part1")
  val part2 = actions.foldLeft(start)((s: State, a: Action) => s.takeActionWaypoint(a)).manhattanDistanceFromOrigin
  println(s"Part 2: $part2")

case class Waypoint(x: Int, y: Int):
  def move(dx: Int, dy: Int): Waypoint = Waypoint(x + dx, y + dy)
  def rotate(rotation: Rotation, times: Int): Waypoint = (1 to times).foldLeft(this) { case (waypoint, _) =>
    rotation match
      case Rotation.Left  => Waypoint(waypoint.y, -waypoint.x)
      case Rotation.Right => Waypoint(-waypoint.y, waypoint.x)
  }

case class State(x: Int, y: Int, heading: Heading, waypoint: Waypoint):
  val manhattanDistanceFromOrigin: Int = x.abs + y.abs
  def takeActionDirect(action: Action): State = action match
    case Forward(d)              => this.copy(x = x + (heading.x * d), y = y + (heading.y * d))
    case Move(dx, dy)            => this.copy(x = x + dx, y = y + dy)
    case Rotate(rotation, times) => this.copy(heading = heading.rotate(rotation, times))
  def takeActionWaypoint(action: Action): State = action match
    case Move(x, y)              => this.copy(waypoint = waypoint.move(x, y))
    case Rotate(rotation, times) => this.copy(waypoint = waypoint.rotate(rotation, times))
    case Forward(magnitude)      => this.copy(x = x + (waypoint.x * magnitude), y = y + (waypoint.y * magnitude))

enum Rotation:
  case Left, Right

sealed trait Heading(val x: Int, val y: Int, val left: Heading, val right: Heading):
  def rotate(rotation: Rotation, times: Int): Heading = (1 to times).foldLeft(this) { (heading: Heading, _) =>
    rotation match
      case Rotation.Left  => heading.left
      case Rotation.Right => heading.right
  }
case object North extends Heading(0, -1, West, East)
case object South extends Heading(0, 1, East, West)
case object West  extends Heading(-1, 0, South, North)
case object East  extends Heading(1, 0, North, South)

sealed trait Action
case class Move(x: Int, y: Int)                   extends Action
case class Rotate(rotation: Rotation, times: Int) extends Action
case class Forward(magnitude: Int)                extends Action
object Action:
  def fromString(s: String): Option[Action] = s.splitAt(1) match
    case ("N", y)         => Some(Move(0, -y.toInt))
    case ("S", y)         => Some(Move(0, y.toInt))
    case ("E", x)         => Some(Move(x.toInt, 0))
    case ("W", x)         => Some(Move(-x.toInt, 0))
    case ("F", magnitude) => Some(Forward(magnitude.toInt))
    case ("L", degrees)   => Some(Rotate(Rotation.Left, (degrees.toInt) / 90))
    case ("R", degrees)   => Some(Rotate(Rotation.Right, (degrees.toInt) / 90))
    case _                => None
