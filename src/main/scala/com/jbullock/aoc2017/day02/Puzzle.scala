package com.jbullock.aoc2017.day02

import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: Vector[Vector[Int]] = Source.fromResource("aoc/2017/Day02/Input.txt").getLines.toVector.map{
    _.replaceAll("\t", ",")
    .split(',')
    .toVector
    .map(_.toInt)
  }
  println(s"Part1: ${Puzzle.part1(input)}")
  println(s"Part2: ${Puzzle.part2(input)}")

object Puzzle:
  def part1(input: Vector[Vector[Int]]): Int =
    input.map(v => v.max - v.min).sum

  def part2(input: Vector[Vector[Int]]): Int =
    input.flatMap{ v =>
      for {
        x <- v
        y <- v if x % y == 0 && x != y
      } yield {
        x/y
      }}.sum

