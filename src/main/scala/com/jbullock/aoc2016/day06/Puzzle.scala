package com.jbullock.aoc2016.day06

import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: Vector[String] = Source.fromResource("aoc/2016/Day06/Input.txt").getLines.toVector
  println(s"Part1: ${Puzzle.part1(input)}")
  println(s"Part2: ${Puzzle.part2(input)}")

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
