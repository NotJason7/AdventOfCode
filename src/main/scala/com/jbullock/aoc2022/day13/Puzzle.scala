package com.jbullock.aoc2022.day13

import scala.annotation.tailrec
import io.circe.Json
import io.circe.parser.parse

@main def solvePuzzle(): Unit =
  val input   = scala.io.Source.fromResource("aoc/2022/Day13/Input.txt").getLines.toVector.filterNot(_.isBlank)
  val pairs   = input.grouped(2).toVector
  val signals = pairs.map(v => v.map(parseJson)).map(v => Signal(v(0), v(1)))
  val part1   = signals.map(_.isOrdered).zipWithIndex.filter(_._1).map(_._2 + 1).sum
  println(s"Part 1: $part1")
  val inputWithDividers   = input ++ Vector("[[2]]", "[[6]]")
  val signalsWithDividers = inputWithDividers.map(parseJson)
  val sorted              = signalsWithDividers.sortWith { case (j: Json, k: Json) => j.isBefore(k) }
  val divider2index       = sorted.indexOf(parseJson("[[2]]")) + 1
  val divider6index       = sorted.indexOf(parseJson("[[6]]")) + 1
  val part2               = divider2index * divider6index
  println(s"Part 2: $part2")

def parseJson(s: String): Json = parse(s) match
  case Right(json) => json
  case Left(e)     => throw RuntimeException(s"Couldn't parse $s as json: $e")
case class Signal(left: Json, right: Json):
  val isOrdered: Boolean = left.isBefore(right)

extension (j: Json)
  def isBefore(k: Json): Boolean = (j.isNumber, k.isNumber) match
    case (true, true) => j.isNumericallyBefore(k)
    case _            => j.isArrayicallyBefore(k)

  def toInt: Int = j.asNumber.flatMap(_.toInt).get

  def toVector: Vector[Json] = j.asArray.get

  def isNumericallyBefore(k: Json): Boolean = j.toInt < k.toInt

  def isArrayicallyBefore(k: Json): Boolean =
    val x = if j.isNumber then Json.fromValues(List(j)).toVector else j.toVector
    val y = if k.isNumber then Json.fromValues(List(k)).toVector else k.toVector
    (x.headOption, y.headOption) match
      case (Some(a), Some(b)) if !a.isBefore(b) => false
//      case (Some(a), Some(b)) =>
//        val n = Json.fromValues(x.tail.toList)
//        val m = Json.fromValues(y.tail.toList)
//        n.isBefore(m)
      case (Some(_), None) => false
      case (None, Some(_)) => true
      case (None, None)    => true
