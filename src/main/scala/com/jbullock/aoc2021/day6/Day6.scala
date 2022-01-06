package com.jbullock.aoc2021.day6

import scala.io.Source
import scala.annotation.tailrec


@main
def runDay6(): Unit = 
  println(s"Part one: ${Day6.part1}")
  println(s"Part two: ${Day6.part2}")
 
object Day6 {
  val input = Source
    .fromResource("2021/Day6Input.txt")
    .getLines
    .mkString
    .split(",")
    .filter(!_.isEmpty)
    .map(_.toInt)

  def part1: Long =
    val initialDemographic =
      input.groupBy(identity).map((k, v) => (k, v.length.toLong))
    population(loop(initialDemographic, project, 80))

  def part2: Long =
    val initialDemographic =
      input.groupBy(identity).map((k, v) => (k, v.length.toLong))
    population(loop(initialDemographic, project, 256))

  def project(current: Map[Int, Long]): Map[Int, Long] =
    Map(
      0 -> current.getOrElse(1, 0L),
      1 -> current.getOrElse(2, 0L),
      2 -> current.getOrElse(3, 0L),
      3 -> current.getOrElse(4, 0L),
      4 -> current.getOrElse(5, 0L),
      5 -> current.getOrElse(6, 0L),
      6 -> (current.getOrElse(7, 0L) + current.getOrElse(0, 0L)),
      7 -> current.getOrElse(8, 0L),
      8 -> current.getOrElse(0, 0L)
    )

  @tailrec
  def loop(
      demographic: Map[Int, Long],
      ageFunction: Map[Int, Long] => Map[Int, Long],
      steps: Int
  ): Map[Int, Long] =
    if steps == 0 then demographic
    else loop(ageFunction(demographic), ageFunction, steps - 1)

  def population(demographic: Map[Int, Long]): Long =
    demographic.foldLeft(0L) { case (a, (k, v)) => a + v }
}

