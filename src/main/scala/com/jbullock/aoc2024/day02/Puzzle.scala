package com.jbullock.aoc2024.day02

@main def solvePuzzle(): Unit =
  val input =
    scala.io.Source
      .fromResource("aoc/2024/Day02/Input.txt")
      .getLines
      .toSeq
      .map(line => line.split(' ').map(_.toInt).toSeq)
  val differences = input.map(_.differences)
  val part1       = differences.count(_.isSafe)
  println(s"Part 1: $part1")
  val part2 =
    input
      .map(levels => levels.possibleDrops.map(_.differences))
      .count(levelList => levelList.exists(levels => levels.isSafe))
  println(s"Part 2: $part2")

extension (levels: Seq[Int])
  def possibleDrops: Seq[Seq[Int]] = levels.indices.map(index => levels.take(index) ++ levels.drop(index + 1))
  def differences: Seq[Int]        = levels.sliding(2).toSeq.map { case Seq(a, b) => a - b }
  def isSafe: Boolean =
    val absolutes = levels.map(_.abs)
    val minDiff   = absolutes.min
    val maxDiff   = absolutes.max
    val signs     = levels.map(_.sign).distinct
    signs.length == 1 && minDiff >= 1 && maxDiff <= 3
