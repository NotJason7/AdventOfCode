package com.jbullock.aoc2017.day04

import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: Vector[Vector[String]] = Source.fromResource("aoc/2017/Day04/Input.txt").getLines.toVector
    .map(_.split(" ").toVector)
  println(s"Part1: ${Puzzle.part1(input)}")
  println(s"Part2: ${Puzzle.part2(input)}")

object Puzzle:
  def part1(input: Vector[Vector[String]]): Int =
    input.count(v => v == v.distinct)

  def part2(input: Vector[Vector[String]]): Int =
    input.map(_.map(_.sorted)).count(v => v == v.distinct)
