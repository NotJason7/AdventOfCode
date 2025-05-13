package com.jbullock.aoc2024.day13

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source
    .fromResource("aoc/2024/Day13/Input.txt")
    .getLines()
    .toSeq
    .filter(_.nonEmpty)
    .grouped(3)
    .toSeq
  val machines = input.map { case List(aString, bString, prizeString) =>
    val a = aString match
      case s"Button A: X+$x, Y+$y" => Button(x.toInt, y.toInt)
    val b = bString match
      case s"Button B: X+$x, Y+$y" => Button(x.toInt, y.toInt)
    val prize = prizeString match
      case s"Prize: X=$x, Y=$y" => Position(x.toInt, y.toInt)
    Machine(a, b, prize)
  }
  val part1 = machines.flatMap(_.determineMinTokens()).sum
  println(s"Part 1: $part1")
  val part2 = machines.flatMap(_.determineMinTokens(true)).sum
  println(s"Part 2: $part2")

case class Position(x: Int, y: Int)

case class Button(x: Int, y: Int)

case class Machine(a: Button, b: Button, prize: Position):
  def determineMinTokens(isPart2: Boolean = false): Option[BigInt] = determineWinningPresses(isPart2) match
    case Some((as, bs)) => Some(as * 3 + bs)
    case None           => None

  private def determineWinningPresses: Option[(Int, Int)] =
    val validCombinations = for
      aPresses <- 0 to 100
      bPresses <- 0 to 100
      if aPresses * a.x + bPresses * b.x == prize.x && aPresses * a.y + bPresses * b.y == prize.y
    yield (aPresses, bPresses)
    if validCombinations.isEmpty then None else Some(validCombinations.minBy((as, bs) => as * 3 + bs))

  private def determineWinningPresses(isPart2: Boolean): Option[(BigInt, BigInt)] =
    /*
    1. A * x1 + B * x2 = z1
    2. A * y1 + B * y2 = z2
    3. A * x1 * y1 + B * x2 * y1 = z1 * y1
    4. A * x1y1 + B * x3 = z1 * y1
    5. A * x1 * y1 + B * y2 * x1 = z2 * x1
    6. A * x1y1 + B * y3 = z2 * x1
    7: 6 - 5 => B * (y3 - x3) = z2 * x1 - z1 * y1
    8. B = (z4 - z3) / (x4 - x3)
    9. A = (z1 - (B * x2)) / x1

     */
    val z1 = if isPart2 then BigInt("10000000000000") + prize.x else BigInt(prize.x)
    val z2 = if isPart2 then BigInt("10000000000000") + prize.y else BigInt(prize.y)
    val B  = (z2 * a.x - z1 * a.y) / (b.y * a.x - b.x * a.y)
    val A  = (z1 - B * b.x) / a.x

    val valid = a.x * A + b.x * B == z1 && a.y * A + b.y * B == z2
    if valid then Some((A, B))
    else None
