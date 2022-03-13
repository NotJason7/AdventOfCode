package com.jbullock.aoc2021.day03

import scala.io.Source
import scala.annotation.tailrec

@main
def solvePuzzle(): Unit =
  Puzzle.part1()
  Puzzle.part2()

object Puzzle {
  val input: List[String] = Source
    .fromResource("aoc/2021/Day03/Input.txt")
    .getLines
    .toList
    .map(_.toString)

  val processedInput: List[String] = input
    .map(_.toList)
    .transpose
    .map(l => l.map(_.toString.toDouble))
    .map(x => x.sum / x.length)
    .map(x => x.round.toString)

  def part1(): Unit =
    val gamma = binaryStringToInt(processedInput)
    val epsilon = Integer.parseInt(
      processedInput.map(x => (1 - x.toInt).toString).fold("")(_ + _),
      2
    )
    val answer = gamma * epsilon
    println(s"Part 1: $answer")

  def part2(): Unit =
    val oxygen = calcOxygen(input)
    val c02 = calcC02(input)
    val answer = oxygen * c02
    println(s"Part 2: $answer")


  def binaryStringToInt(input: List[String]): Int =
    Integer.parseInt(input.fold("")(_ + _), 2)

  def processInput(binaryList: List[String]): List[Int] =
    binaryList
      .map(_.toList)
      .transpose
      .map(l => l.map(_.toString.toDouble))
      .map(x => x.sum / x.length)
      .map(x => x.round.toInt)

  def calcOxygen(input: List[String]): Int =

    @tailrec
    def loop(index: Int, remaining: List[String]): List[String] =
      if remaining.length == 1 then remaining
      else
        val processedInput = processInput(remaining)
        val filteredRemaining =
          remaining.filter(x =>
            x(index).toString != processedInput(index).toString
          )
        loop(index + 1, filteredRemaining)

    val binaryString = loop(0, input)
    binaryStringToInt(binaryString)

  def calcC02(input: List[String]): Int =

    @tailrec
    def loop(index: Int, remaining: List[String]): List[String] =
      if remaining.length == 1 then remaining
      else
        val processedInput = processInput(remaining)
        val filteredRemaining =
          remaining.filter(x =>
            x(index).toString != (1 - processedInput(index).toInt).toString
          )
        loop(index + 1, filteredRemaining)

    val binaryString = loop(0, input)
    binaryStringToInt(binaryString)

}
