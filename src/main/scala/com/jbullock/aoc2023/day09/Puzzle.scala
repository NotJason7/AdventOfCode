package com.jbullock.aoc2023.day09

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input     = scala.io.Source.fromResource("aoc/2023/Day09/Input.txt").getLines.toVector
  val histories = input.map(_.split(" ").toVector.map(_.toInt))
  val part1     = histories.map(_.extrapolate(true)).sum
  println(s"Part 1: $part1")
  val part2 = histories.map(_.extrapolate(false)).sum
  println(s"Part 2: $part2")

extension (is: Vector[Int])
  def extrapolate(isForward: Boolean): Int = down(is, Vector.empty[Int], isForward)

@tailrec def down(values: Vector[Int], levels: Vector[Int], isForward: Boolean): Int =
  val oneLevelDown = values.sliding(2).toVector.flatMap { case Vector(a, b) => Vector(b - a) }
  val nextLevels   = if isForward then levels :+ values.last else values.head +: levels
  if oneLevelDown.distinct.size == 1 then
    if isForward then up(oneLevelDown.last, (a, b) => a + b, nextLevels)
    else up(oneLevelDown.head, (a, b) => a - b, nextLevels)
  else down(oneLevelDown, nextLevels, isForward)
@tailrec def up(i: Int, upFunction: (Int, Int) => Int, levels: Vector[Int]): Int =
  if levels.isEmpty then i else up(upFunction.apply(levels.head, i), upFunction, levels.tail)
