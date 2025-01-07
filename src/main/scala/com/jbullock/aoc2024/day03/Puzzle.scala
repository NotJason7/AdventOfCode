package com.jbullock.aoc2024.day03

import scala.annotation.tailrec
import scala.util.matching.Regex

@main def solvePuzzle(): Unit =
  val input              = scala.io.Source.fromResource("aoc/2024/Day03/Input.txt").getLines.toSeq.mkString
  val parsedInstructions = input.findAllInstructionStrings
  val part1 = parsedInstructions.map {
    case Mul(a, b) => a * b
    case Do        => 0
    case Dont      => 0
  }.sum
  println(s"Part 1: $part1")
  val part2 = parsedInstructions
    .foldLeft(State(0, true)) { (state: State, i: Instruction) =>
      i match
        case Mul(a, b) if state.isDoing => State(state.total + a * b, state.isDoing)
        case Do                         => State(state.total, true)
        case _                          => State(state.total, false)
    }
    .total
  println(s"Part 2: $part2")

case class State(total: Int, isDoing: Boolean)

extension (s: String)
  def findAllInstructionStrings: Seq[Instruction] =
    val instructionRegEx = "mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)".r
    instructionRegEx.findAllMatchIn(s).toSeq.map(_.toString).flatMap(Instruction.fromString)

sealed trait Instruction
object Instruction:
  def fromString(s: String): Option[Instruction] = s match
    case "do()"        => Some(Do)
    case "don't()"     => Some(Dont)
    case s"mul($a,$b)" => Some(Mul(a.toInt, b.toInt))
    case _             => None

case class Mul(a: Int, b: Int) extends Instruction:
  assert(a >= 0 && a <= 999)
  assert(b >= 0 && b <= 999)
case object Do   extends Instruction
case object Dont extends Instruction
