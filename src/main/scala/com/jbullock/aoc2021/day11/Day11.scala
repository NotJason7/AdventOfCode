package com.jbullock.aoc2021.day11

import scala.annotation.tailrec
import scala.io.Source

@main
def runDay11(): Unit =
  Day11.part1()
  Day11.part2()

case class Position(x: Int, y: Int)

extension (p: Position) {
  def adjacent: List[Position] =
    val adjacentPositions = List(
      ( 1,  1),
      ( 1,  0),
      ( 1, -1),
      ( 0,  1),
      ( 0, -1),
      (-1,  1),
      (-1,  0),
      (-1, -1)
    )
    adjacentPositions.map((xx, yy) => Position(p.x + xx, p.y + yy))
}

case class Octopus(energy: Int, hasFlashed: Boolean, totalFlashes: Int)

extension (o: Octopus) {
  def increment: Octopus =
    Octopus(o.energy + 1, false, o.totalFlashes)
  def flash: Octopus =
    Octopus(0, true, o.totalFlashes + 1)
  def canFlash: Boolean =
    o.energy > 9 && !o.hasFlashed
}

object Day11:
  val input: List[List[Int]] = Source
    .fromResource("2021/Day11/Day11Input.txt")
    .getLines
    .toList
    .map(_.toList.map(_.asDigit))
  val xRange: List[Int] = (1 to input.head.size).toList
  val yRange: List[Int] = (1 to input.size).toList
  val grid: Map[Position, Octopus] = (
    for {
      y <- yRange
      x <- xRange
    } yield Position(x, y) -> Octopus(input(y-1)(x-1), false, 0)
  ).toMap


  def part1(): Unit =
    val steps = 100
    val gridAfter = timeSteps(steps, grid)
    val totalFlashes = gridAfter.values.map(o => o.totalFlashes).sum
    println(s"Part one: $totalFlashes")

  def part2(): Unit =
    val firstSyncFlashStep = findFirstSyncFlash(0, grid)
    println(s"Part two: $firstSyncFlashStep")

  @tailrec
  def findFirstSyncFlash(stepCount: Int, grid: Map[Position, Octopus]): Int =
    if grid.filter((_, octopus) => octopus.hasFlashed).size == grid.keys.size then stepCount
    else findFirstSyncFlash(stepCount + 1, timeSteps(1, grid))

  @tailrec
  def timeSteps(steps: Int, grid: Map[Position, Octopus]): Map[Position, Octopus] =
    if steps < 1 then grid
    else
      val incrementGridEnergy = grid.map{ (p, o) => (p, o.increment) }

      @tailrec
      def flashLoop(grid: Map[Position, Octopus]): Map[Position, Octopus] =
        val flashed = grid
          .filter((_, o) => o.canFlash)
          .map((p, o) => p -> o.flash)
        if flashed.keys.size == 0 then grid
        else
          val flashedGrid = flashed.foldLeft(grid)(_ + _)
          val energyGain = flashed.keys.toList
            .flatMap(_.adjacent)
            .filter( p => flashedGrid.keys.toList.contains(p) && !flashedGrid(p).hasFlashed)
            .groupBy(identity)
            .map((p, v) => p -> Octopus(flashedGrid(p).energy + v.size, false, flashedGrid(p).totalFlashes))
          val nextGrid = energyGain.foldLeft(flashedGrid)(_ + _)
          flashLoop(nextGrid)

      val endStep = flashLoop(incrementGridEnergy)

      timeSteps(steps - 1, endStep)

  def viewGrid(grid: Map[Position, Octopus]): Unit =
    val array = yRange.map(y => xRange.map(x => Position(x, y)))
    val arrayValues = array.map(_.map(p => grid(p).energy).mkString).mkString(sep="\n")
    println(arrayValues)
