package com.jbullock.aoc2020.day01

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2020/Day01/Input.txt").getLines.toVector.map(_.toInt)
  println(s"Part 1: ${findSumGiveProduct(2, 2020, input)}")
  println(s"Part 2: ${findSumGiveProduct(3, 2020, input)}")

def findSumGiveProduct(nElements: Int, targetSum: Int, numbers: Vector[Int]): Option[Int] =
  numbers
    .combinations(nElements)
    .find(_.sum == targetSum)
    .map(_.product)
