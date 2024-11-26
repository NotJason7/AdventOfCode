package com.jbullock.aoc2021_2.day07

@main def solvePuzzle(): Unit =
  val input = scala.io.Source
    .fromResource("aoc/2021/Day07/Day7Input.txt")
    .getLines()
    .flatMap(_.split(',').toSeq.map(_.toInt))
    .toSeq
  val possibleSpots = (input.min to input.max)
  val part1         = possibleSpots.map(input.distanceFrom).min
  val part2         = possibleSpots.map(input.increasingDistanceFrom).min
  println(s"Part 1: $part1")
  println(s"Part 2: $part2")

extension (xs: Seq[Int])
  def distanceFrom(dx: Int): Int           = xs.map(x => (dx - x).abs).sum
  def increasingDistanceFrom(dx: Int): Int = xs.map(x => (1 to (dx - x).abs).sum).sum
