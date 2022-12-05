package com.jbullock.aoc2022.day04

@main def solvePuzzle(): Unit =
  val input = io.Source.fromResource("aoc/2022/Day04/Input.txt").getLines.toVector
  val numbersRegEx = "[0-9]+".r
  val numbers = input.map(s => numbersRegEx.findAllIn(s).toVector.map(_.toInt))
  val assignments = numbers.map(Assignment.fromVector)
  val fullOverlaps = assignments.count(_.fullOverlap)
  println(s"Part 1: $fullOverlaps")
  val anyOverlaps = assignments.count(_.anyOverlap)
  println(s"Part 2: $anyOverlaps")

case class Assignment(aStart: Int, aEnd: Int, bStart: Int, bEnd: Int):
  val a: Set[Int] = (aStart to aEnd).toSet
  val b: Set[Int] = (bStart to bEnd).toSet
  val fullOverlap: Boolean = a.intersect(b) == a || b.intersect(a) == b
  val anyOverlap: Boolean = a.intersect(b).nonEmpty
object Assignment:
  def fromVector(v: Vector[Int]): Assignment =
    if v.length != 4 then throw new RuntimeException(s"Assignment vectors must have 4 elements, you tried ${v.length}.")
    else
      val Vector(aStart, aEnd, bStart, bEnd) = v
      Assignment(aStart, aEnd, bStart, bEnd)
