package com.jbullock.aoc2016.Day06

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: Vector[String] = Source.fromResource("aoc/2016/Day06/Input.txt").getLines.toVector
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
  val part2Answer = Puzzle.part2(input)
  println(s"Part2: $part2Answer")

object Puzzle:
  def part1(input: Vector[String]): String = input
    .map(_.toVector)
    .transpose
    .map(_.groupBy(identity).maxBy(_._2.size))
    .map(_._1)
    .mkString

  def part2(input: Vector[String]): String = input
    .map(_.toVector)
    .transpose
    .map(_.groupBy(identity).minBy(_._2.size))
    .map(_._1)
    .mkString
