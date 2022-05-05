package com.jbullock.aoc2016.day09

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: Vector[String] = Source.fromResource("aoc/2016/Day09/Input.txt").getLines.toVector
  println(s"Part1: ${Puzzle.part1(input)}")
  println(s"Part2: ${Puzzle.part2(input)}")

object Puzzle:
  def part1(input: Vector[String]): BigInt =
    decompressedLength(input.head, false)

  def part2(input: Vector[String]): BigInt =
    decompressedLength(input.head, true)

def decompressedLength(compressed: String, recursiveDecompress: Boolean) =

  @tailrec
  def loop(total: BigInt, compressed: String): BigInt =
    compressed.split("[?<=()]", 3) match
      case Array(prefix: String, actionString: String, suffix: String) =>
        val action = Action.fromString(actionString)
        val actionText = suffix.take(action.length)
        val additionalLength =
          if recursiveDecompress then action.applyRecursive(actionText)
          else action.applyAction(actionText)
        val newTotal = total + prefix.length + additionalLength
        val stillCompressed = suffix.drop(action.length)
        loop(newTotal, stillCompressed)
      case Array(prefix) =>
        total + prefix.length

  loop(0, compressed)

case class Action(length: Int, repeats: Int):
  def applyAction(s: String): BigInt =
    repeats * length + s.drop(length).length
  def applyRecursive(s: String): BigInt =
    repeats * decompressedLength(s.take(length), true) + s.drop(length).length
object Action:
  def fromString(s: String): Action =
    s.replaceAll("[()]","").split("x").toList.map(_.toInt) match
      case List(length: Int, repeats: Int) => Action(length, repeats)
      case _ => throw new RuntimeException("Invalid Action")
