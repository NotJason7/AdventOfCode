package com.jbullock.aoc2021.day01

import scala.io.Source

@main
def solvePuzzle: Unit =
  Puzzle.part1
  Puzzle.part2

object Puzzle {
  val input: List[Int] = Source
    .fromResource("aoc/2021/Day01/Input.txt")
    .getLines
    .toList
    .map(_.toInt)

  def countIncreasing(list: List[Int]): Int =
    val start = (0, list(0))
    list.drop(1).foldLeft(start){
      case ((count, last), current) =>
        val increment = if current > last then 1 else 0
        (count + increment  , current)
    }._1

  def part1: Unit =
    val count = countIncreasing(input)
    println(s"Part 1: $count")

  def part2: Unit =
    val range = 0 to (input.length - 3)
    val window = range.map(x => input(x) + input(x+1) + input(x+2)).toList
    val count = countIncreasing(window)
    println(s"Part 2: $count")
}
