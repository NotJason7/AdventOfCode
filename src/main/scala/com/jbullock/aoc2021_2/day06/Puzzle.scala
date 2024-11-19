package com.jbullock.aoc2021_2.day06

@main def solvePuzzle(): Unit =
  val input = scala.io.Source
    .fromResource("aoc/2021/Day06/Day6Input.txt")
    .getLines
    .toSeq
    .flatMap(
      _.split(',')
        .map(_.toInt)
    )
  val start: Map[Int, BigInt] = input.groupBy(identity).view.mapValues(count => BigInt(count.size)).toMap
  val part1                   = (1 to 80).foldLeft(start)((pop, _) => pop.next).values.sum
  val part2                   = (1 to 256).foldLeft(start)((pop, _) => pop.next).values.sum
  println(part1)
  println(part2)

extension (currentPopulation: Map[Int, BigInt])
  def next: Map[Int, BigInt] =
    (0 to 8).foldLeft(Map.empty[Int, BigInt])((population, counter) =>
      counter match
        case 6 =>
          population.updated(6, currentPopulation.getOrElse(7, BigInt(0)) + currentPopulation.getOrElse(0, BigInt(0)))
        case 8 => population.updated(8, currentPopulation.getOrElse(0, BigInt(0)))
        case n => population.updated(n, currentPopulation.getOrElse(n + 1, BigInt(0)))
    )
