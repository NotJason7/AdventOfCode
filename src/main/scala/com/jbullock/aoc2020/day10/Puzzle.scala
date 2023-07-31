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

/*
a single optional one that leads to a fixed point creates a 2 branch
1 2 3
1   3

two optionals in a row creates a 4 branch
1 2 3 4
1 2   4
1   3 4
1     4

three optionals in a row creates a 7 branch
1 2 3 4 5
1   3 4 5
1     4 5
1 2   4 5
1 2     5
1 2 3   5
1   3   5

four optionals in a row creates 13 branches
1 2 3 4 5 6

1   3 4 5 6
1 2   4 5 6
1 2 3   5 6
1 2 3 4   6

1 2 3     6
1 2   4   6
1 2     5 6
1   3 4   6
1   3   5 6
1     4 5 6

1   3     6
1     4   6

6 can be reached by 5(7) + 4(4) + 3(2) = 13
5 can be reached by 4(4) + 3(2) + 2(1) = 7
4 can be reached by 3(2) + 2(1) + 1(1) = 4
3 can be reached by 2(1) + 1(1) = 2
2 can be reached by 1
*/