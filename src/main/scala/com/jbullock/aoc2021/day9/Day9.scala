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
  def get(pos: Position): Int = matrix(Position.y)(Position.x)
  def safeGet(pos: Position): Option[Int] = pos match
    case Position(x, y) if 0 <= x && x < matrix(0).length && 0 <= y && y < matrix.length => Some(matrix(y)(x))
    case _ => None

case class Position(x: Int, y: Int) 


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
      if isPositionLocalMin(matrix, x, y)
    yield matrix.safeGet(x, y).get
    val minTotal = localMinValues.map(_+1).sum
    println(s"Part one: $minTotal")
  
  def part2(): Unit =
    val matrix = Matrix(input)  
    val localMinValues = for
      x <- (0 until matrix.xMax)
      y <- (0 until matrix.yMax)
      val pos = Position(x, y)
      if isPositionLocalMin(matrix, pos)
    yield matrix.get(pos)

  def isPositionLocalMin(matrix: Matrix, pos: Position): Boolean =

    def compare(point: Int, comparison: Position): Boolean = 
      val 
      comparison match
        case Some(comparisonInt) => point < comparisonInt
        case None => true

    val posVal = matrix.get(pos)
    val up = Position(pos.x, pos.y+1)
    val down = Position(pos.x, pos.y-1)
    val left = Position(pos.x-1, pos.y)
    val right = Position(pos.x+1, pos.y)

    val upVal = matrix.safeGet(up)
    val downVal = matrix.safeGet(down)
    val leftVal = matrix.safeGet(left)
    val rightVal = matrix.safeGet(right) 

    val up = compare(posVal, matrix.safeGet(x, y-1))
    val down = compare(posVal, matrix.safeGet(x, y+1))
    val left = compare(posVal, matrix.safeGet(x-1, y))
    lazy val right = compare(posVal, matrix.safeGet(x+1, y))
    up && down && left && right

  // def findBasinSize(matrix: Matrix, localX: Int, localY: Int): Int =

  //   def loop(search)

    
