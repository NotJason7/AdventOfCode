package com.jbullock.aoc2022.day13

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input  = scala.io.Source.fromResource("aoc/2022/Day13/Input.txt").getLines.toVector.filterNot(_.isBlank)
  val tokens = input.map(formatSignal)
  val sorted = tokens.grouped(2).toVector.map(v => v(0).isSorted(v(1)))
  val part1  = sorted.zipWithIndex.filter(_._1).map(_._2 + 1).sum
  println(s"Part 1: $part1") //5588
  val divider2           = Vector("[", "[", "2", "]", "]")
  val divider6           = Vector("[", "[", "6", "]", "]")
  val tokensWithDividers = input.map(formatSignal) ++ Vector(divider2, divider6)
  val sortedWithDividers = tokensWithDividers.sortWith(_.isSorted(_))
  val divider2index      = sortedWithDividers.indexOf(divider2) + 1
  val divider6index      = sortedWithDividers.indexOf(divider6) + 1
  val part2              = divider2index * divider6index
  println(s"Part 2: $part2")

def formatSignal(s: String) = s
  .replace("[", ",[,")
  .replace("]", ",],")
  .split(',')
  .toVector
  .filterNot(_.isBlank)

extension (leftVector: Vector[String])
  def isSorted(rightVector: Vector[String]): Boolean =
    @tailrec def loop(left: Vector[String], right: Vector[String]): Boolean =
      (left.headOption, right.headOption) match
        case (Some(l), Some(r)) =>
          if l == r then loop(left.tail, right.tail)
          else if l == "]" then true
          else if r == "]" then false
          else if l == "[" then loop(left.tail, right)
          else if r == "[" then loop(left, right.tail)
          else l.toInt <= r.toInt
        case (Some(l), None) => false
        case _               => true

    loop(leftVector, rightVector)
