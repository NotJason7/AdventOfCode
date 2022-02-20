package com.jbullock.aoc2021.day09

import scala.annotation.tailrec
import scala.io.Source

@main
def runDay9(): Unit =
  Day9.part1()
  Day9.part2()

case class Matrix(matrix: Vector[Vector[Int]]):
  val xMax = matrix(0).length
  val xRange = (0 until xMax)
  val yMax = matrix.length
  val yRange = (0 until yMax)
  def get(position: Position): Int = matrix(position.y)(position.x)
  def safeGet(position: Position): Option[Int] = position match
    case Position(x, y) if 0 <= x && x < matrix(0).length && 0 <= y && y < matrix.length => Some(matrix(y)(x))
    case _ => None

case class Position(x: Int, y: Int):
  def getValidCardinalPositions(using matrix: Matrix): List[Position] =
    val up = Position(x, y-1)
    val down = Position(x, y+1)
    val left = Position(x-1, y)
    val right = Position(x+1, y)
    List(up, down, left, right).filter { z =>
      matrix.safeGet(z) match
        case Some(_) => true
        case None => false
    }

  def value(using matrix: Matrix): Int =
    matrix.get(this)

  def isLocalMinimum(using matrix: Matrix): Boolean =
    this.getValidCardinalPositions
      .map(cardinal => this.value < cardinal.value)
      .foldLeft(true)(_ && _)

  def toBasinPoints(using matrix: Matrix): List[Position] =
    @tailrec
    def loop(toCheck: List[Position], inBasin: List[Position], checkedPositions: List[Position]): List[Position] =
      if toCheck.isEmpty then inBasin
      else
        val current = toCheck.head
        val next = if current.value < 9 then
          current.getValidCardinalPositions.filterNot((checkedPositions ++ toCheck.tail).contains(_)) ++ toCheck.tail
          else toCheck.tail
        val nextInBasin = if current.value < 9
          then List(current) ++ inBasin
          else inBasin
        val nextCheckedPositions = List(current) ++ checkedPositions
        loop(next, nextInBasin, nextCheckedPositions)

    loop(List(this), List[Position](), List[Position]())

object Day9:
  val input = Source
    .fromResource("2021/Day09/Day9Input.txt")
    .getLines
    .toVector
    .map(_.toVector.map(_.toString.toInt))

  def part1(): Unit =
    given matrix: Matrix = Matrix(input)
    val positions = for
      x <- matrix.xRange
      y <- matrix.yRange
    yield Position(x, y)
    val localMinima = positions.filter(_.isLocalMinimum)
    val minTotal = localMinima.map(matrix.get(_)+1).sum
    println(s"Part one: $minTotal")
  
  def part2(): Unit =
    given matrix: Matrix = Matrix(input)
    val positions = for
      x <- matrix.xRange
      y <- matrix.yRange
    yield Position(x, y)
    val localMinima = positions.filter(_.isLocalMinimum)
    val basinSizeProduct = localMinima.map(_.toBasinPoints)
      .map(_.length)
      .sortWith(_ > _)
      .take(3)
      .product
    println(s"Part two: $basinSizeProduct")







    
