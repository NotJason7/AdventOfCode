package com.jbullock.aoc2016.day01

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: List[String] = Source
    .fromResource("aoc/2016/Day01/Input.txt")
    .getLines
    .toList
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
  val part2Answer = Puzzle.part2(input)
  println(s"Part2: $part2Answer")

enum Direction:
  case Right
  case Left

case class Movement(direction: Direction, magnitude: Int)

object Movement:
  def fromString(s: String): Movement =
    val m = s.splitAt(1)
    m match
      case ("L", n) => Movement(Direction.Left, n.toInt)
      case ("R", n) => Movement(Direction.Right, n.toInt)
      case _ => throw new Exception("Movement was not Left or Right")

case class Position(x: Int, y: Int)
extension(p: Position)
  def move(change: (Int, Int)): Position =
    Position(p.x + change._1, p.y + change._2)
  def originDistance(): Int =
    math.abs(p.x) + math.abs(p.y)
  def positionFill(target: Position): List[Position] =
    if p.x == target.x then
      val direction = if p.y < target.y then 1 else -1
      (p.y to target.y by direction).toList.map(Position(p.x, _)).filterNot(_ == p)
    else
      val direction = if p.x < target.x then 1 else -1
      (p.x to target.x by direction).toList.map(Position(_, p.y)).filterNot(_ == p)

sealed trait Heading:
  def left(n: Int): Heading
  def right(n: Int): Heading
  def position(): Position
case class North(p: Position) extends Heading:
  override def left(n: Int): Heading  = West(Position(p.x - n, p.y))
  override def right(n: Int): Heading = East(Position(p.x + n, p.y))
  override def position(): Position = p
case class South(p: Position) extends Heading:
  override def left(n: Int): Heading  = East(Position(p.x + n, p.y))
  override def right(n: Int): Heading = West(Position(p.x - n, p.y))
  override def position(): Position = p
case class East(p: Position) extends Heading:
  override def left(n: Int): Heading  = North(Position(p.x, p.y + n))
  override def right(n: Int): Heading = South(Position(p.x, p.y - n))
  override def position(): Position = p
case class West(p: Position) extends Heading:
  override def left(n: Int): Heading  = South(Position(p.x, p.y - n))
  override def right(n: Int): Heading = North(Position(p.x, p.y + n))
  override def position(): Position = p


def trackAndMove(movements: List[Movement], heading: Heading): Position =

  @tailrec
  def loop(movements: List[Movement], heading: Heading, positions: List[Position]): Position =
    val movement = movements.head
    val nextHeading = movement.direction match
      case Direction.Left =>
        heading.left(movement.magnitude)
      case Direction.Right =>
        heading.right(movement.magnitude)
    val newPositions = heading.position().positionFill(nextHeading.position())
    val revisitedPositions = newPositions.filter(positions.contains(_))
    if revisitedPositions.nonEmpty then revisitedPositions.head
    else loop(movements.tail, nextHeading, positions ++ newPositions)

  loop(movements, heading, List(heading.position()))

object Puzzle:

  def part1(input: List[String]): Int =
    val startPosition = Position(0, 0)
    val startHeading = North(startPosition)
    val movements = input
      .head
      .replace(" ", "")
      .split(",")
      .toList
      .map(Movement.fromString)
    val finalHeading = movements.foldLeft(startHeading){
      (h: Heading, m: Movement) => m.direction match
        case Direction.Left => h.left(m.magnitude)
        case Direction.Right => h.right(m.magnitude)
    }
    finalHeading.position().originDistance()

  def part2(input: List[String]): Int =
    val startPosition = Position(0, 0)
    val startHeading = North(startPosition)
    val movements = input
      .head
      .replace(" ", "")
      .split(",")
      .toList
      .map(Movement.fromString)
    val firstRepeatedPosition = trackAndMove(movements, startHeading)
    firstRepeatedPosition.originDistance()





