package com.jbullock.aoc2020.day13

@main def solvePuzzle(): Unit =
  val input        = scala.io.Source.fromResource("aoc/2020/Day13/Input.txt").getLines.toVector
  val earliestTime = input.head.toInt
  val busses       = input.tail.head.split(",").filterNot(_ == "x").map(i => Bus(i.toInt)).toVector
//  val busMinDepartures = busses.map(_.departureAfter(earliestTime))
//  val earliestDepartingBus = busses.minBy(_.departureAfter(earliestTime))
  val part1 = busses.minBy(_.departureAfter(earliestTime)).waitTimesId(earliestTime)
  println(s"Part 1: $part1")

case class Bus(id: Int):
  def waitTimesId(minute: Int): Int    = (departureAfter(minute) - minute) * id
  def departureAfter(minute: Int): Int = (id - (minute % id)) + minute
