package com.jbullock.aoc2017.day05

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input = Source.fromResource("aoc/2017/Day05/Input.txt").getLines.toVector.map(_.toInt)
  println(s"Part1: ${Puzzle.part1(input)}")
  println(s"Part2: ${Puzzle.part2(input)}")

object Puzzle:
  def part1(input: Vector[Int]): Int = State(0, 0, input).jumps(x => x + 1)
  def part2(input: Vector[Int]): Int = State(0, 0, input).jumps(x => if x >= 3 then x - 1 else x + 1)

case class State(steps: Int, position: Int, instructions: Vector[Int]):
  @tailrec
  final def jumps(rule: Int => Int): Int = instructions.lift(position) match
    case Some(p) => State(steps + 1, position + p, instructions.updated(position, rule(p))).jumps(rule)
    case None => steps
