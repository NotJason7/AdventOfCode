package com.jbullock.aoc2021.day07

import scala.io.Source

@main
def runDay7(): Unit = 
  println(s"Part one: ${Day7.part1()}")
  println(s"Part two: ${Day7.part2()}")


object Day7 {
  val input = Source
    .fromResource("aoc/2021/Day07/Day7Input.txt")
    .getLines
    .mkString
    .split(",")
    .toList
    .map(_.toInt)

  def part1(): Int =
    val fuelUsage = (input.min to input.max).toList.map{ x =>
      val fuel = calculateFuel(input, x)
      (x, fuel)
    }
    fuelUsage.minBy(_._2)._2


  def part2(): Int =
    val fuelUsage = (input.min to input.max).toList.map{ x =>
      val fuel = calculateIncrementalFuel(input, x)
      (x, fuel)
    }
    fuelUsage.minBy(_._2)._2

  def calculateFuel(positions: List[Int], targetPosition: Int): Int =
    positions.map(x => (x - targetPosition).abs).sum

  def calculateIncrementalFuel(positions: List[Int], targetPosition: Int): Int =
    positions.map{ x =>
      val distance = (x - targetPosition).abs
      (1 to distance).sum
    }.sum

}
