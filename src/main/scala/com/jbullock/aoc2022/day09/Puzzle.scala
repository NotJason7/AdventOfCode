package com.jbullock.aoc2022.day09

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2022/Day09/Input.txt").getLines.toVector
  val moves = input.map(Move.fromString)
  val part1 = moves.foldLeft(Rope.ofLengthN(2))((rope, move) => rope.makeMove(move)).tailVisited.size
  println(s"Part 1: $part1")
  val part2 = moves.foldLeft(Rope.ofLengthN(10))((rope, move) => rope.makeMove(move)).tailVisited.size
  println(s"Part 2: $part2")

sealed trait Direction(val x: Int, val y: Int)
case object Up    extends Direction(0, -1)
case object Down  extends Direction(0, 1)
case object Left  extends Direction(-1, 0)
case object Right extends Direction(1, 0)

case class Position(x: Int, y: Int):
  def move(d: Direction): Position           = Position(x + d.x, y + d.y)
  def moveToward(p: Position): Position      = Position(x + (p.x - x).sign, y + (p.y - y).sign)
  def distancesFrom(p: Position): (Int, Int) = ((x - p.x).abs, (y - p.y).abs)
object Position:
  val origin: Position = Position(0, 0)

case class Move(direction: Direction, amount: Int):
  def next: Move = this.copy(amount = amount - 1)
object Move:
  def fromString(s: String): Move =
    val Vector(directionString, amountString) = s.split(' ').toVector
    val direction = directionString match
      case "U" => Up
      case "D" => Down
      case "L" => Left
      case "R" => Right
    Move(direction, amountString.toInt)

case class Rope(knots: Vector[Position], tailVisited: Set[Position]):
  @tailrec final def makeMove(move: Move): Rope = if move.amount == 0 then this
  else
    val start = Vector(knots.head.move(move.direction))
    val nextKnots = knots.tail.foldLeft(start) { (updatedKnots, tail) =>
      val head = updatedKnots.last
      val updatedKnot = head.distancesFrom(tail) match
        case (x, y) if x > 0 && y > 0 && x + y >= 3    => tail.moveToward(head)
        case (x, y) if (x == 0 ^ y == 0) && x + y >= 2 => tail.moveToward(head)
        case _                                         => tail
      updatedKnots :+ updatedKnot
    }
    val nextVisited = tailVisited + nextKnots.last
    Rope(nextKnots, nextVisited).makeMove(move.next)
object Rope:
  def ofLengthN(n: Int): Rope = Rope(Vector.fill(n)(Position.origin), Set.empty[Position])
