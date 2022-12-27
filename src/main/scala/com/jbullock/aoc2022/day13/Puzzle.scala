package com.jbullock.aoc2022.day13

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input   = scala.io.Source.fromResource("aoc/2022/Day13/Input.txt").getLines.toVector.filterNot(_.isBlank)
  val tokens  = input.map(_.replace("10", "t"))
  val signals = tokens.grouped(2).zipWithIndex
  val part1   = signals.collect { case (Vector(left, right), index) if isInOrder(left, right) => index + 1 }.sum
  println(s"Part 1: $part1")
  val dividers           = Vector("[[2]]", "[[6]]")
  val tokensWithDividers = tokens ++ dividers
  val sorted             = tokensWithDividers.sortWith(isInOrder)
  val part2              = dividers.map(sorted.indexOf(_) + 1).product
  println(s"Part 2: $part2")

@tailrec def isInOrder(left: String, right: String): Boolean = (left.head, right.head) match
  case (l, r) if l == r => isInOrder(left.tail, right.tail)
  case (']', _)         => true
  case (_, ']')         => false
  case ('[', r)         => isInOrder(left.tail, s"$r]" ++ right.tail)
  case (l, '[')         => isInOrder(s"$l]" ++ left.tail, right.tail)
  case (l, r)           => l < r
