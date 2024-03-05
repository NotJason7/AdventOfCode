package com.jbullock.aoc2023.day06

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2023/Day06/Sample.txt").getLines.toVector
//  val races = Vector(Race(57, 291), Race(72, 1172), Race(69, 1176), Race(92, 2026))
  val races = Vector(Race(7d, 9d), Race(15d, 40d), Race(30d, 200d))
  val part1 = races.map(_.winnablePresses).product.toInt
  println(s"Part 1: $part1")
//  val part2Race = Race(BigDecimal(71530), BigDecimal(940200))
  val part2Race = Race(BigDecimal(57726992), BigDecimal(291117211762026d))
  val part2     = part2Race.winnablePresses
  println(s"Part 2: $part2")

case class Race(duration: BigDecimal, distance: BigDecimal):
  def winnablePresses: BigDecimal = (for
    min <- minWinnablePress
    max <- maxWinnablePress
    presses = (max - min + 1)
  yield presses).getOrElse(0)
  extension (pressDurations: LazyList[BigDecimal])
    private def findFirstBeyondDistance: Option[BigDecimal] = pressDurations.find(pressDuration =>
      val moveDuration = duration - pressDuration
      val moveDistance = pressDuration * moveDuration
      moveDistance > distance
    )
  private def minWinnablePress: Option[BigDecimal] =
    LazyList.from(BigDecimal(0) to duration by BigDecimal(1)).findFirstBeyondDistance
  private def maxWinnablePress: Option[BigDecimal] =
    LazyList.from(duration to BigDecimal(0) by BigDecimal(-1)).findFirstBeyondDistance
