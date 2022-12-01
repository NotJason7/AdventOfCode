package com.jbullock.aoc2022.day01

import annotation.tailrec

@main def solvePuzzle(): Unit =
  val input: Vector[String] = io.Source.fromResource("aoc/2022/Day01/Input.txt").getLines.toVector
  val elfTotalCalories = input
    .appended("")
    .foldLeft((0, Vector.empty[Int]))((accumulator: (Int, Vector[Int]), calorieAmountString: String) =>
      accumulator match
        case (accumulatedCalories: Int, elfTotals: Vector[Int]) =>
          if calorieAmountString.isEmpty then (0, elfTotals :+ accumulatedCalories)
          else (accumulatedCalories + calorieAmountString.toInt, elfTotals)
    )
    ._2
  val maxCalories = elfTotalCalories.max
  println(s"Part 1a: $maxCalories")
  val top3Calories = elfTotalCalories.sortWith(_ > _).take(3).sum
  println(s"Part 2a: $top3Calories")

  val recursiveTotal = sumElfCalories(input)
  val part1b         = recursiveTotal.max
  println(s"Part 1b: $part1b")
  val part2b         = recursiveTotal.sortWith(_ > _).take(3).sum
  println(s"Part 2b: $part2b")

def sumElfCalories(calorieStrings: Vector[String]): Vector[Int] =

  @tailrec def loop(calorieStrings: Vector[String], currentElfTotal: Int, elfTotalCalories: Vector[Int]): Vector[Int] =
    if calorieStrings.isEmpty then elfTotalCalories
    else
      val next = calorieStrings.head
      if next.isEmpty then loop(calorieStrings.tail, 0, elfTotalCalories :+ currentElfTotal)
      else loop(calorieStrings.tail, currentElfTotal + next.toInt, elfTotalCalories)

  loop(calorieStrings, 0, Vector.empty[Int])
