package com.jbullock.aoc2020.day03

@main def solvePuzzle(): Unit =
  given Vector[String] = scala.io.Source.fromResource("aoc/2020/Day03/Input.txt").getLines.toVector
  val part1            = Trajectory(3).treesHit
  println(s"Part 1: $part1")
  val part2 = List(Trajectory(1), Trajectory(3), Trajectory(5), Trajectory(7), Trajectory(1, 2)).map(_.treesHit).product
  println(s"Part 2: $part2")

case class Position(x: Int, y: Int)
case class Trajectory(x: Int, y: Int = 1):
  def positions(width: Int, n: Int): Seq[Position] = (0 until n).map(i => Position((i * x) % width, i * y))
  def treesHit(using map: Vector[String]): Int =
    positions(map.head.length, map.size / y).count(p => map(p.y)(p.x) == '#')
