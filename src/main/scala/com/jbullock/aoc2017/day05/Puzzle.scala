package com.jbullock.aoc2017.day05

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  println(s"Part 1: ${Puzzle.part1}")
  println(s"Part 2: ${Puzzle.part2}")

object Puzzle:
  val input: Vector[Int] = Source.fromResource("aoc/2017/Day05/Input.txt").getLines.toVector.map(_.toInt)
  def part1: Int = State(0, 0, input).jumps(_ + 1)
  def part2: Int = State(0, 0, input).jumps(x => if x >= 3 then x - 1 else x + 1)

case class State(steps: Int, position: Int, instructions: Vector[Int]):
  @tailrec
  final def jumps(rule: Int => Int): Int = instructions.lift(position) match
    case Some(p) => State(steps + 1, position + p, instructions.updated(position, rule(p))).jumps(rule)
    case None => steps
