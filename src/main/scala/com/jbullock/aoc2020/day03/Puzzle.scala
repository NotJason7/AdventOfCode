package com.jbullock.aoc2020.day03

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input                 = scala.io.Source.fromResource("aoc/2020/Day03/Input.txt").getLines.toVector
  given Vector[Vector[Int]] = input.map(_.toVector.map(c => if c == '#' then 1 else 0))
  val part1                 = countHitTrees(Trajectory(3))
  println(s"Part 1: $part1")
  val part2 = List(Trajectory(1), Trajectory(3), Trajectory(5), Trajectory(7), Trajectory(1, 2))
    .map(countHitTrees)
    .product
  println(s"Part 2: $part2")

case class Trajectory(x: Int, y: Int = 1)
def countHitTrees(t: Trajectory)(using treeMap: Vector[Vector[Int]]): Int =
  @tailrec
  def loop(map: Vector[Vector[Int]], x: Int = 0, trees: Int = 0): Int =
    map.headOption match
      case None => trees
      case Some(treeRow) =>
        val nextX = (x + t.x) % treeRow.size
        loop(map.drop(t.y), nextX, trees + treeRow(nextX))
  loop(treeMap.drop(t.y))
