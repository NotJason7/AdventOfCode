package com.jbullock.aoc2020.day03

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2020/Day03/Input.txt").getLines.toVector
  val part1 = countHitTrees(input, Trajectory(3, 1))
  println(s"Part 1: $part1")
  val part2 = List(
    Trajectory(1, 1),
    Trajectory(3, 1),
    Trajectory(5, 1),
    Trajectory(7, 1),
    Trajectory(1, 2)
  ).map(t => countHitTrees(input, t)).product
  println(s"Part 2: $part2")

case class Trajectory(right: Int, down: Int)
def countHitTrees(map: Vector[String], trajectory: Trajectory): Int =
  val repeatWidth = map.head.length
  @tailrec
  def loop(currentMap: Vector[String], currentX: Int, trees: Int): Int =
    if currentMap.isEmpty then trees
    else
      val nextX  = (currentX + trajectory.right) % repeatWidth
      val onTree = if currentMap.head(nextX) == '#' then 1 else 0
      loop(currentMap.drop(trajectory.down), nextX, trees + onTree)
  loop(map.drop(trajectory.down), 0, 0)
