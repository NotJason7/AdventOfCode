package com.jbullock.aoc2024.day01

@main def solvePuzzle(): Unit =
  val input: Seq[(Int, Int)] = scala.io.Source.fromResource("aoc/2024/Day01/Input.txt").getLines.toSeq.map {
    case s"$left   $right" => (left.toInt, right.toInt)
  }
  val lefts: Seq[Int]        = input.map(_._1).sortWith(_ < _)
  val rights: Seq[Int]       = input.map(_._2).sortWith(_ < _)
  val distances: Seq[Int]    = lefts.zip(rights).map((left, right) => (left - right).abs)
  val similarities: Seq[Int] = lefts.map(a => a * rights.count(_ == a))
  val part1: Int             = distances.sum
  val part2: Int             = similarities.sum
  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
