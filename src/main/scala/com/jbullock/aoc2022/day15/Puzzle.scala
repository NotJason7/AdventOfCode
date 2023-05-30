package com.jbullock.aoc2022.day15

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2022/Day15/Input.txt").getLines.toVector
//  val input = scala.io.Source.fromResource("aoc/2022/Day15/Sample.txt").getLines.toVector
  val sensors = input.map("-?[0-9]+".r.findAllIn(_).map(_.toInt).toVector).map { case Vector(x1, y1, x2, y2) =>
    Sensor(Position(x1, y1), Position(x2, y2))
  }
  val part1 = sensors.flatMap(_.pingedPositions).count(_.y == 2000000)
  println(s"Part 1: $part1")

case class Sensor(position: Position, closestBeacon: Position):
  private val beaconDistance: Int = position.distanceFrom(closestBeacon)
  val pingedPositions: Set[Position] =
    (for
      x <- (-beaconDistance to beaconDistance).map(_ + position.x)
      y <- (-beaconDistance to beaconDistance).map(_ + position.y)
      if (position.x - x).abs + (position.y - y).abs <= beaconDistance
    yield Position(position.x + x, position.y + y)).toSet

case class Position(x: Int, y: Int):
//  val tuningFrequency: Int           = (x * 4000000) + y
  def distanceFrom(p: Position): Int = (p.x - x).abs + (p.y - y).abs
  override def toString              = s"($x,$y)"
