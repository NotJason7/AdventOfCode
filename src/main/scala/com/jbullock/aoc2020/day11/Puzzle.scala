package com.jbullock.aoc2020.day11

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2020/Day11/Input.txt").getLines.toVector
  val positionStateMap = (for
    y <- input.indices
    x <- input.head.indices
    position = Position(x, y)
    space    = if input(y)(x) == 'L' then Space.Empty else Space.Floor
  yield position -> space).toMap
  val part1 = State(positionStateMap, Seating.Adjacent, 4).nextUntilStable.countOccupied
  println(s"Part 1: $part1")
  val part2 = State(positionStateMap, Seating.Visible, 5).nextUntilStable.countOccupied
  println(s"Part 2: $part2")

enum Seating:
  case Adjacent, Visible

enum Space:
  case Empty, Occupied, Floor

case class State(seatMap: Map[Position, Space], seating: Seating, occupiedThreshold: Int):
  private val minX: Int = seatMap.keys.map(_.x).min
  private val maxX: Int = seatMap.keys.map(_.x).max
  private val minY: Int = seatMap.keys.map(_.y).min
  private val maxY: Int = seatMap.keys.map(_.y).max
  def draw(): Unit = for
    y <- minY to maxY
    line = (minX to maxX)
      .map(x => seatMap(Position(x, y)))
      .map {
        case Space.Empty    => 'L'
        case Space.Occupied => '#'
        case Space.Floor    => '.'
      }
    _ = println(line.mkString(" "))
  yield ()
  def countOccupied: Int = seatMap.count { (_, space) => space == Space.Occupied }
  def next: State =
    val nextSeatMap: Map[Position, Space] = seatMap.keys.map { position =>
      val relevantOccupiedSeats = position.findSeats(seatMap, seating).count(_ == Space.Occupied)
      seatMap(position) match
        case Space.Empty =>
          if relevantOccupiedSeats == 0 then position -> Space.Occupied else position -> Space.Empty
        case Space.Occupied =>
          if relevantOccupiedSeats >= occupiedThreshold then position -> Space.Empty else position -> Space.Occupied
        case Space.Floor => position -> Space.Floor
    }.toMap
    State(nextSeatMap, seating, occupiedThreshold)
  @tailrec final def nextUntilStable: State =
    val nextState = this.next
    if this == nextState then this else nextState.nextUntilStable

trait Direction(val x: Int, val y: Int)
object Direction:
  val all: Vector[Direction] = Vector(Up, Down, Left, Right, UpRight, UpLeft, DownRight, DownLeft)
  private case object Up        extends Direction(0, -1)
  private case object Down      extends Direction(0, 1)
  private case object Left      extends Direction(-1, 0)
  private case object Right     extends Direction(1, 0)
  private case object UpRight   extends Direction(1, -1)
  private case object UpLeft    extends Direction(-1, -1)
  private case object DownRight extends Direction(1, 1)
  private case object DownLeft  extends Direction(-1, 1)

case class Position(x: Int, y: Int):
  private def moveDirection(d: Direction): Position = Position(x + d.x, y + d.y)
  @tailrec private final def moveDirectionUntilSeat(d: Direction, seatMap: Map[Position, Space]): Option[Position] =
    val nextPosition = moveDirection(d)
    seatMap.get(nextPosition) match
      case Some(space) if space == Space.Floor => nextPosition.moveDirectionUntilSeat(d, seatMap)
      case Some(_)                             => Some(nextPosition)
      case None                                => None

  private def adjacentPositions: Vector[Position] = Direction.all.map(d => moveDirection(d))

  private def visibleSeats(seatMap: Map[Position, Space]): Vector[Position] =
    Direction.all.flatMap(d => moveDirectionUntilSeat(d, seatMap))

  def findSeats(seatMap: Map[Position, Space], seating: Seating): Vector[Space] = seating match
    case Seating.Adjacent => adjacentPositions.flatMap(seatMap.get)
    case Seating.Visible  => visibleSeats(seatMap).flatMap(seatMap.get)
