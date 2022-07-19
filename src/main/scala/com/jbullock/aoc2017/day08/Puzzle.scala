package com.jbullock.aoc2017.day08

import scala.io.Source

@main
def solvePuzzle(): Unit =
  println(s"Part 1: ${Puzzle.part1}")
  println(s"Part 2: ${Puzzle.part2}")

object Puzzle:
  val input: Vector[Instruction] = Source.fromResource("aoc/2017/Day08/Example.txt").getLines.toVector.map(Instruction
    .fromString)
  println(input)
  val output: State = input.foldLeft(State(0, Map.empty[String, Int]))((s, i) => i.makeChange(s))
  println(output)

  def part1: Int = output.registers.values.max
  def part2: Int = output.max


case class State(max: Int, registers: Map[String, Int])

case class Condition(name: String, test: Int => Boolean)
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

case class Instruction(change: Change, condition: String):
  def evaluateCondition(state: State): Boolean =
    condition.split(" ", 2).toVector match
      case Vector(name, test) =>
        val register = state.registers.getOrElse(name, 0)
        test.split(" ", 2).toVector match
          case Vector(bool, value) =>
            val comparison = value.toInt
            bool match
              case ">" => register > comparison
              case "<" => register < comparison
              case ">=" => register >= comparison
              case "==" => register == comparison
              case "<=" => register <= comparison
              case "!=" => register != comparison
              case _ => throw new RuntimeException(s"Unable to compare using bool $bool")
          case _ => throw new RuntimeException(s"Unable to evaluate boolean test $test")
      case _ => throw new RuntimeException(s"Unable to evaluate condition $condition")
  def makeChange(state: State): State =
    if evaluateCondition(state) then change.evaluate(state)
    else state
object Instruction:
  def fromString(s: String): Instruction =
    s.split(" if ").toVector match
      case Vector(change, condition) => Instruction(Change.fromString(change), condition)
      case _ => throw new RuntimeException(s"Unable to parse instruction $s")

case class Change(name: String, value: Int):
  def evaluate(s: State): State =
    val next = s.registers.getOrElse(name, 0) + value
    val max = Set(next, s.max).max
    State(max, s.registers.updated(name, next))
object Change:
  def fromString(s: String): Change = s.split(" ", 2).toVector match
    case Vector(name, update) =>
      val value = update.split(" ", 2).toVector match
        case Vector(direction, magnitude) => direction match
          case "inc" => magnitude.toInt
          case "dec" => -magnitude.toInt
        case _ => throw new RuntimeException(s"Unable to parse update $update")
      Change(name, value)
    case _ => throw new RuntimeException(s"Unable to parse change $s")
