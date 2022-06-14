package com.jbullock.aoc2017.day01

import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: String = Source.fromResource("aoc/2017/Day01/Input.txt").getLines.toVector.head
  println(s"Part1: ${Puzzle.part1(input)}")
  println(s"Part2: ${Puzzle.part2(input)}")

object Puzzle:
  def part1(input: String): Int =
    input.appended(input.head).sliding(2).toVector.map(str => if str(0) == str(1) then str(0).asDigit else 0).sum

  def part2(input: String): Int =
    val toCompare = input.repeat(2).slice(input.length/2, input.length + (input.length /2))
    input.zip(toCompare).map(x => if x._1 == x._2 then x._1.asDigit else 0).sum
