package com.jbullock.aoc2020.day06

@main def solvePuzzle(): Unit =
  val input   = scala.io.Source.fromResource("aoc/2020/Day06/Input.txt").getLines.toVector
  val surveys = input.mkString(",").split(",,").toVector
  val part1   = surveys.map(s => s.replace(",", "").toSet).map(_.size).sum
  println(s"Part 1: $part1")
  val part2 = surveys.map(_.split(',').toVector.map(_.toSet).reduce(_.intersect(_)).size).sum
  println(s"Part 2: $part2")
