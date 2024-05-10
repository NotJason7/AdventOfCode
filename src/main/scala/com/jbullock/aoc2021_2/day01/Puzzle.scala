package com.jbullock.aoc2021_2.day01

@main def solvePuzzle(): Unit =
  val input: Seq[Int] = scala.io.Source.fromResource("aoc/2021/Day01/Input.txt").getLines.toSeq.map(_.toInt)
  val part1           = input.countIncreasing
  println(s"Part 1: $part1")
  val part2 = input.sliding(3).map(_.sum).toVector.countIncreasing
  println(s"Part 2: $part2")

extension (is: Seq[Int]) def countIncreasing: Int = is.sliding(2).count { case Seq(a, b) => a < b }