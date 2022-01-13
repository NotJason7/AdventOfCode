package com.jbullock.aoc2021.day9

import scala.io.Source

@main
def runDay9(): Unit =
  Day9.part1()
  // println(s"Part one: ${Day9.part1()}")
  // println(s"Part two: ${Day9.part2()}")

case class Matrix(val matrix: Vector[Vector[Int]]):
  val xMax = matrix(0).length
  val xRange = (0 until xMax)
  val yMax = matrix.length
  val yRange = (0 until yMax)
  def safeGet(x: Int, y: Int): Option[Int] = (x, y) match
    case (x, y) if 0 <= x && x < matrix(0).length && 0 <= y && y < matrix.length => Some(matrix(y)(x))
    case _ => None

case class Coordinate(x: Int, y: Int) 


object Day9:
  val input = Source
    .fromResource("2021/Day9Input.txt")
    .getLines
    .toVector
    .map(_.toVector.map(_.toString.toInt))

  def part1(): Unit =
    val matrix = Matrix(input)  
    val localMinValues = for
      x <- (0 until matrix.xMax)
      y <- (0 until matrix.yMax)
      if isCoordinateLocalMin(matrix, x, y)
    yield matrix.safeGet(x, y).get
    println(localMinValues)
    val minTotal = localMinValues.map(_+1).sum
    println(s"Part one: $minTotal")
  
  def part2(): Unit =
    ???

  def isCoordinateLocalMin(matrix: Matrix, x: Int, y: Int): Boolean =

    def compare(point: Int, comparison: Option[Int]): Boolean = 
      comparison match
        case Some(comparisonInt) => point < comparisonInt
        case None => true

    val point = matrix.matrix(y)(x)
    val up = compare(point, matrix.safeGet(x, y-1))
    lazy val down = compare(point, matrix.safeGet(x, y+1))
    lazy val left = compare(point, matrix.safeGet(x-1, y))
    lazy val right = compare(point, matrix.safeGet(x+1, y))
    up && down && left && right

    
