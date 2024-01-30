package com.jbullock.aoc2023.day03

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input     = scala.io.Source.fromResource("aoc/2023/Day03/Input.txt").getLines.toVector
  val schematic = Schematic(input)
  val part1     = schematic.partNumbers.map(_.partNumber).sum
  println(s"Part 1: $part1")
  val part2 = schematic.gearRatios.sum
  println(s"Part 2: $part2")

case class Point(x: Int, y: Int):
  def adjacentPoints: Vector[Point] =
    for
      dy <- (-1 to 1).toVector
      dx <- (-1 to 1)
      if dx != 0 || dy != 0
    yield Point(x + dx, y + dy)
  def hasAdjacentSymbol(pointMap: Map[Point, Char]): Boolean = adjacentPoints.exists(adjacent =>
    val c = pointMap.getOrElse(adjacent, ',')
    !c.isDigit && c != '.'
  )

case class PartNumber(partNumber: Int, points: Vector[Point]):
  def adjacentGears(pointMap: Map[Point, Char]): Vector[Point] =
    points.flatMap(_.adjacentPoints).filter(point => pointMap.get(point).contains('*')).distinct
object PartNumber:
  def isValid(numberString: String, points: Vector[Point], pointMap: Map[Point, Char]): Boolean =
    numberString.nonEmpty && points.exists(_.hasAdjacentSymbol(pointMap))

case class Schematic(schematicRows: Vector[String]):
  private val pointMap: Map[Point, Char] =
    (for
      y <- schematicRows.indices
      x <- (0 until schematicRows.head.length)
    yield Point(x, y) -> schematicRows(y)(x)).toMap

  val partNumbers: Vector[PartNumber] =
    schematicRows.zipWithIndex.flatMap { (string, y) =>

      @tailrec
      def loop(
          stringSoFar: String,
          number: String,
          points: Vector[Point],
          partNumbers: Vector[PartNumber]
      ): Vector[PartNumber] =
        stringSoFar.headOption match
          case None if PartNumber.isValid(number, points, pointMap) => partNumbers :+ PartNumber(number.toInt, points)
          case None                                                 => partNumbers
          case Some(nextChar) if nextChar.isDigit =>
            val nextNumberString = number + nextChar.toString
            val nextPoints       = points :+ Point(string.length - stringSoFar.length, y)
            loop(stringSoFar.drop(1), nextNumberString, nextPoints, partNumbers)
          case _ if PartNumber.isValid(number, points, pointMap) =>
            loop(stringSoFar.drop(1), "", Vector.empty[Point], partNumbers :+ PartNumber(number.toInt, points))
          case _ => loop(stringSoFar.drop(1), "", Vector.empty[Point], partNumbers)

      loop(string, "", Vector.empty[Point], Vector.empty[PartNumber])
    }

  private val partNumberGears: Seq[(PartNumber, Vector[Point])] = partNumbers
    .map(partNumber => (partNumber, partNumber.adjacentGears(pointMap)))
    .filter((_, gears) => gears.nonEmpty)

  val gearRatios: Seq[Int] = partNumberGears
    .flatMap(_._2)
    .filter(gear => partNumberGears.count((_, gears) => gears.contains(gear)) == 2)
    .distinct
    .map(gear => partNumberGears.filter(_._2.contains(gear)).map(_._1.partNumber).product)
