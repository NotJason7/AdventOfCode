package com.jbullock.aoc2020.day02

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2020/Day02/Input.txt").getLines.toVector
  val policyPasswords = input.map { case s"${min}-${max} $char: $password" =>
    (Policy(min.toInt, max.toInt, char.head), password)
  }
  val part1 = policyPasswords.count { case (policy, password) => policy.isValidCount(password) }
  println(s"Part 1: $part1")
  val part2 = policyPasswords.count { case (policy, password) => policy.hasValidPositions(password) }
  println(s"Part 2: $part2")

extension (i: Int) def isInclusivelyBetween(x: Int, y: Int): Boolean = x <= i && i <= y

case class Policy(min: Int, max: Int, character: Char):
  def isValidCount(password: String): Boolean      = password.count(c => c == character).isInclusivelyBetween(min, max)
  def hasValidPositions(password: String): Boolean = password(min - 1) == character ^ password(max - 1) == character
