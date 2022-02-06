package com.jbullock.aoc2021.day02

import scala.io.Source

@main
def solvePuzzle: Unit =
  Puzzle.part1
//  Puzzle.part2

case class Instruction(direction: String, magnitude: Int)

extension (i: Instruction) {
  def toDirection: (Int, Int) =
    i.direction match {
      case "forward" => (i.magnitude, 0)
      case "up" => (0, -i.magnitude)
      case "down" => (0, i.magnitude)
      case _ => (0, 0)
    }
}

object Puzzle {
  val input = Source
    .fromResource("2021/Day02/Input.txt")
    .getLines
    .toList
    .map {
      _.split(" ") match
        case Array(direction, magnitude) => Instruction(direction, magnitude.toInt)
    }

  def part1: Unit =
    val directions = input.map(_.toDirection)
    val totalMovement = directions.foldLeft((0, 0)){
      case ((ax, ay), (x, y)) => (ax + x, ay + y)
    }
    val answer = totalMovement._1 * totalMovement._2
    println(s"Part 1: $answer")


}
