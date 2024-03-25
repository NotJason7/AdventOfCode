package com.jbullock.aoc2023.day09

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input     = scala.io.Source.fromResource("aoc/2023/Day09/Input.txt").getLines.toVector
  val histories = input.map(_.split(" ").toVector.map(_.toInt))
  val part1     = histories.map(_.extrapolateForward).sum
  println(s"Part 1: $part1")
  val part2 = histories.map(_.extrapolateBackward).sum
  println(s"Part 2: $part2")

extension (integers: Vector[Int])
  def extrapolateForward: Int  = extrapolateDown(integers, direction = Direction.Forward)
  def extrapolateBackward: Int = extrapolateDown(integers, direction = Direction.Backward)

enum Direction:
  case Forward, Backward

@tailrec def extrapolateDown(
    is: Vector[Int],
    levelStarts: Vector[Int] = Vector.empty[Int],
    levelEnds: Vector[Int] = Vector.empty[Int],
    direction: Direction
): Int =
  val oneLevelDown    = is.sliding(2).toVector.flatMap { case Vector(a, b) => Vector(b - a) }
  val nextLevelStarts = is.head +: levelStarts
  val nextLevelEnds   = levelEnds :+ is.last
  if oneLevelDown.toSet.size == 1 then
    if direction == Direction.Forward then extrapolateUpForward(oneLevelDown.last, nextLevelEnds)
    else extrapolateUpBackward(oneLevelDown.head, nextLevelStarts)
  else extrapolateDown(oneLevelDown, nextLevelStarts, nextLevelEnds, direction)
@tailrec def extrapolateUpForward(i: Int, levelsToClimb: Vector[Int]): Int =
  if levelsToClimb.isEmpty then i else extrapolateUpForward(levelsToClimb.head + i, levelsToClimb.tail)
@tailrec def extrapolateUpBackward(i: Int, levelsToClimb: Vector[Int]): Int =
  if levelsToClimb.isEmpty then i else extrapolateUpBackward(levelsToClimb.head - i, levelsToClimb.tail)
