package com.jbullock.aoc2017.day22

import scala.io.Source

@main def solvePuzzle(): Unit =
  val infections = Source
    .fromResource("aoc/2017/Day22/Input.txt")
    .getLines
    .map(_.map(c => if c == '#' then Infected else Clean))
    .toVector
  val startNodes = (for {
    y <- infections.indices
    x <- infections.head.indices
  } yield Point(x, y) -> infections(y)(x)).toMap
  val startCarrier = Carrier(Up, Point(infections.head.length / 2, infections.length / 2))

  val part1Rules = Map(Clean -> Infected, Infected -> Clean)
  val part1Start = State(startCarrier, startNodes, 0, part1Rules)
  val part1End   = runN(10000, part1Start)
  println(s"Part 1: ${part1End.infections}")

  val part2Rules = Map(Clean -> Weakened, Weakened -> Infected, Infected -> Flagged, Flagged -> Clean)
  val part2Start = State(startCarrier, startNodes, 0, part2Rules)
  val part2End   = runN(10000000, part2Start)
  println(s"Part 2: ${part2End.infections}")

def runN(n: Int, state: State): State = (1 to n).foldLeft(state)((s, _) => s.next)

sealed trait Heading(
    val x: Int,
    val y: Int,
    val clockwise: Heading,
    val counterClockwise: Heading,
    val reverse: Heading
)
case object Up    extends Heading(0, -1, Right, Left, Down)
case object Right extends Heading(1, 0, Down, Up, Left)
case object Down  extends Heading(0, 1, Left, Right, Up)
case object Left  extends Heading(-1, 0, Up, Down, Right)

case class Point(x: Int, y: Int):
  def move(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

sealed trait Status(val symbol: Char, val turn: Heading => Heading)
case object Clean    extends Status('.', (h: Heading) => h.counterClockwise)
case object Weakened extends Status('W', (h: Heading) => h)
case object Infected extends Status('#', (h: Heading) => h.clockwise)
case object Flagged  extends Status('F', (h: Heading) => h.reverse)

case class Carrier(heading: Heading, point: Point):
  def move: Carrier = this.copy(point = point.move(heading.x, heading.y))

case class State(
    carrier: Carrier,
    nodes: Map[Point, Status],
    infections: Int,
    statusOrder: Map[Status, Status]
):
  def next: State =
    val currentStatus  = this.nodes.getOrElse(carrier.point, Clean)
    val nextStatus     = statusOrder.getOrElse(currentStatus, Clean)
    val nextNodes      = nodes.updated(carrier.point, nextStatus)
    val nextCarrier    = Carrier(currentStatus.turn(carrier.heading), carrier.point).move
    val nextInfections = infections + (if nextStatus == Infected then 1 else 0)
    State(nextCarrier, nextNodes, nextInfections, statusOrder)
  def draw(): Unit =
    val xs = nodes.keys.map(p => p.x).min to nodes.keys.map(p => p.x).max
    val ys = nodes.keys.map(p => p.y).min to nodes.keys.map(p => p.y).max
    println((for {
      y <- ys
      x <- xs
      c = nodes.getOrElse(Point(x, y), Clean).symbol + " "
    } yield c).grouped(xs.size).map(_.mkString).mkString("\n"))
