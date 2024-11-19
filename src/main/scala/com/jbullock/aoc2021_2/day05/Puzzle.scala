package com.jbullock.aoc2021_2.day05

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2021/Day05/Day5Input.txt").getLines.toSeq
  val lines = input.map { case s"$x1,$y1 -> $x2,$y2" =>
    Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  }
  val horiVertiCovers              = lines.filter(line => line.isHorizontal || line.isVertical).flatMap(_.covers)
  val horiVertiCoveredMoreThanOnce = horiVertiCovers.groupBy(identity).count(_._2.size >= 2)
  println(s"Part 1: $horiVertiCoveredMoreThanOnce")
  val allCovers              = lines.flatMap(_.covers)
  val allCoveredMoreThanOnce = allCovers.groupBy(identity).count(_._2.size >= 2)
  println(s"Part 1: $allCoveredMoreThanOnce")

case class Point(x: Int, y: Int)

case class Line(x1: Int, y1: Int, x2: Int, y2: Int):
  def isHorizontal: Boolean = y1 == y2
  def isVertical: Boolean   = x1 == x2
  val xs: Seq[Int] =
    if isVertical then (0 to (y2 - y1).abs).map(_ => x1)
    else if x2 > x1 then x1 to x2
    else (x2 to x1).reverse
  val ys: Seq[Int] =
    if isHorizontal then (0 to (x2 - x1).abs).map(_ => y1)
    else if y2 > y1 then y1 to y2
    else (y2 to y1).reverse
  def covers: Seq[Point] =
    xs.zip(ys).map((x, y) => Point(x, y))
