package com.jbullock.aoc2020.day15

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2020/Day15/Input.txt").mkString.split(',').map(_.toInt).toVector
  val part1 = nthNumberSpoken(2020, input)
  println(part1)
  val part2 = nthNumberSpoken(30000000, input)
  println(part2)

def nthNumberSpoken(n: Int, startingNumbers: Vector[Int]): Int =
  val startingMap = startingNumbers.dropRight(1).zipWithIndex.map { case (x, index) => (x, index + 1) }.toMap

  @tailrec def loop(index: Int, number: Int, runningIndexes: Map[Int, Int]): Int =
    if index == n then number
    else
      val nextNumber = if runningIndexes.contains(number) then index - runningIndexes.getOrElse(number, 0) else 0
      loop(index + 1, nextNumber, runningIndexes + (number -> index))

  loop(startingNumbers.length, startingNumbers.last, startingMap)
