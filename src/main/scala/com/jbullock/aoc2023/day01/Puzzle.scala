package com.jbullock.aoc2023.day01

@main def solvePuzzle(): Unit =
  val input: Vector[String] = scala.io.Source.fromResource("aoc/2023/Day01/Input.txt").getLines.toVector
  val part1 = input
    .map(_.filter(_.isDigit))
    .map(is => s"${is.head}${is.last}".toInt)
    .sum
  println(s"Part 1: $part1")
  given digitMapping: Map[String, String] = Map(
    "zero"  -> "0",
    "one"   -> "1",
    "two"   -> "2",
    "three" -> "3",
    "four"  -> "4",
    "five"  -> "5",
    "six"   -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine"  -> "9"
  )
  val digits = digitMapping.keys.toVector ++ digitMapping.values
  val part2  = input.map(s => s"${s.findFirst(digits).toDigit}${s.findLast(digits).toDigit}".toInt).sum
  println(s"Part 2: $part2")

extension (s: String)
  def toDigit(using digitNames: Map[String, String]): String = digitNames.getOrElse(s, s)
  def findFirst(searchTerms: Vector[String]): String = searchTerms.filter(s.contains).minBy(s.indexOf)
  def findLast(searchTerms: Vector[String]): String  = s.reverse.findFirst(searchTerms.map(_.reverse)).reverse
