package com.jbullock.aoc2016.day09

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: Vector[String] = Source.fromResource("aoc/2016/Day09/Input.txt").getLines.toVector
  println(s"Part1: ${Puzzle.part1(input)}")
  println(s"Part2: ${Puzzle.part2(input)}")

object Puzzle:
  def part1(input: Vector[String]): Int =
    input.map(decompress).head.length

  def part2(input: Vector[String]): BigInt =
    chainDecompress(input.head)



def chainDecompress(compressed: String) =

  def loop(total: BigInt, compressed: String): BigInt =
    compressed.split("[?<=()]", 3) match
      case Array(prefix: String, actionString: String, suffix: String) =>
        val lengthSoFar = total + BigInt(prefix.length)
        val action = Action.fromString(actionString)
        val suffixToActOn = suffix.take(action.length)
        val leftToProcess = suffix.drop(action.length)
        val additionalLength = action.applyRecursive(suffixToActOn)
        val newTotal = lengthSoFar + additionalLength
        loop(newTotal, leftToProcess)
      case Array(prefix) =>
        total + prefix.length

  loop(0, compressed)



def decompress(compressed: String): String =
  @tailrec
  def loop(decompressed: String, compressedOption: Option[String]): String =
    compressedOption match
      case None => decompressed
      case Some(compressed) =>
        val (nextDecompressed, nextCompressedOption) = PartialDecompressed.fromString(compressed).processAction
        loop(decompressed + nextDecompressed, nextCompressedOption)
  loop("", Some(compressed))

case class Action(length: Int, repeats: Int):
  def applyAction(s: String): (String, String) =
    val decompressed = s.take(length).repeat(repeats)
    val remainder = s.drop(length)
    (decompressed, remainder)
  def applyRecursive(s: String): BigInt =
    repeats * chainDecompress(s.take(length)) + s.drop(length).length

object Action:
  def fromString(s: String): Action =
    s.replaceAll("[()]","").split("x").toList.map(_.toInt) match
      case List(length: Int, repeats: Int) => Action(length, repeats)
      case _ => throw new RuntimeException("Invalid Action")


case class PartialDecompressed(prefix: String, action: Option[Action], suffix: Option[String]):
  def processAction: (String, Option[String]) =
    (action, suffix) match
      case (Some(action), Some(suffix)) =>
        val (decompressed, compressed) = action.applyAction(suffix)
        (prefix + decompressed, Some(compressed))
      case _ => (prefix, None)
object PartialDecompressed:
  def fromString(s: String): PartialDecompressed =
    s.split("[?<=()]", 3) match
      case Array(prefix: String, actionString: String, suffix: String) =>
        val action = Action.fromString(actionString)
        PartialDecompressed(prefix, Some(action), Some(suffix))
      case _ => PartialDecompressed(s, None, None)
