package com.jbullock.aoc2020.day10

@main def solvePuzzle(): Unit =
  val input              = scala.io.Source.fromResource("aoc/2020/Day10/Input.txt").getLines.toVector.map(_.toInt)
  val joltages           = (input :+ 0 :+ input.max + 3).sortWith(_ < _).map(_.toLong)
  val joltageDifferences = joltages.sliding(2).map(v => v.last - v.head).toVector
  val part1              = joltageDifferences.count(_ == 1) * joltageDifferences.count(_ == 3)
  println(s"Part 1: $part1")
  val steps          = joltages.map(joltage => Step(joltage, joltages.filter(j => j < joltage && j >= joltage - 3)))
  val stepPathCounts = steps.foldLeft(Map.empty[Long, Long]) { case (stepPaths, step) => step.addPathCount(stepPaths) }
  val part2          = stepPathCounts(joltages.max)
  println(s"Part 2: $part2")

case class Step(joltage: Long, reachedBy: Vector[Long]):
  def addPathCount(linkPaths: Map[Long, Long]): Map[Long, Long] =
    if reachedBy.isEmpty then linkPaths + (joltage -> 1L)
    else linkPaths + (joltage                      -> reachedBy.map(j => linkPaths.getOrElse(j, 1L)).sum)
