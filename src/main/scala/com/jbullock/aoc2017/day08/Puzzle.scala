package com.jbullock.aoc2017.day08

import scala.io.Source

@main
def solvePuzzle(): Unit =
  println(s"Part 1: ${Puzzle.part1}")
  println(s"Part 2: ${Puzzle.part2}")

object Puzzle:
  val input: Vector[Instruction] = Source.fromResource("aoc/2017/Day08/Input.txt").getLines.toVector.map(Instruction
    .fromString)
  val output: State = input.foldLeft(State(0, Map.empty[String, Int]))((s, i) => i.followInstruction(s))
  def part1: Int = output.registers.values.max
  def part2: Int = output.max

case class State(max: Int, registers: Map[String, Int])

case class Change(name: String, value: Int):
  def evaluate(s: State): State =
    val next = s.registers.getOrElse(name, 0) + value
    State(Set(next, s.max).max, s.registers.updated(name, next))
object Change:
  def fromString(s: String): Change = s.split(" ", 2).toVector match
    case Vector(name, update) =>
      val value = update.split(" ", 2).toVector match
        case Vector(direction, magnitude) => if direction.startsWith("dec") then -magnitude.toInt else magnitude.toInt
        case _ => throw new RuntimeException(s"Unable to parse update $update")
      Change(name, value)
    case _ => throw new RuntimeException(s"Unable to parse change $s")

case class Condition(name: String, test: Int => Boolean):
  def evaluate(s: State): Boolean = test(s.registers.getOrElse(name,0))
object Condition:
  def fromString(s: String): Condition = s.split(" ", 2).toVector match
    case Vector(name, testString) =>
      testString.split(" ", 2).toVector match
        case Vector(symbol, value) =>
          val comparison = value.toInt
          val test = symbol match
            case ">" =>  (i: Int) => i > comparison
            case "<" =>  (i: Int) => i < comparison
            case ">=" => (i: Int) => i >= comparison
            case "==" => (i: Int) => i == comparison
            case "<=" => (i: Int) => i <= comparison
            case "!=" => (i: Int) => i != comparison
            case _ => throw new RuntimeException(s"Unable to parse symbol $symbol")
          Condition(name, test)
        case _ => throw new RuntimeException(s"Unable to evaluate boolean test $testString")
    case _ => throw new RuntimeException(s"Unable to evaluate condition $s")

case class Instruction(change: Change, condition: Condition):
  def followInstruction(s: State): State = if condition.evaluate(s) then change.evaluate(s) else s
object Instruction:
  def fromString(s: String): Instruction =
    s.split(" if ").toVector match
      case Vector(change, condition) => Instruction(Change.fromString(change), Condition.fromString(condition))
      case _ => throw new RuntimeException(s"Unable to parse instruction $s")
