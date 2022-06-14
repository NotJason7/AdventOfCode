package com.jbullock.aoc2017.day03

import scala.annotation.tailrec

@main
def solvePuzzle(): Unit =
  val input = 265149
  println(s"Part1: ${Puzzle.part1(input)}")
  println(s"Part2: ${Puzzle.part2(input)}")

object Puzzle:
  def part1(input: Int): Int =
    val square = math.ceil(math.sqrt(input))
    val squareEnd = math.pow(square, 2)
    val previousSquare = math.floor(math.sqrt(input))
    val squareStart = math.pow(previousSquare, 2) + 1
    val stepsToSquareEdge = math.floor(square/2).toInt
    val stepsToCornersFromEdge = math.ceil(square/2) - 1
    val largeEdgeLanding = squareEnd - stepsToCornersFromEdge
    val smallEdgeLanding = squareStart + stepsToCornersFromEdge
    val stepsOnEdge = List((largeEdgeLanding - input).abs, (smallEdgeLanding - input).abs).min.toInt
    stepsToSquareEdge + stepsOnEdge

  def part2(input: Int): Int =
    val positionsOrder = LazyList.from(0).flatMap(layerPositions)
    findFirstBiggerThan(positionsOrder, input)

  def findFirstBiggerThan(positions: LazyList[Position], input: Int): Int =

    @tailrec
    def loop(positions: LazyList[Position], grid: Map[Position, Int], checked: Vector[Position]): Int =
      val p = positions.head
      val pValue = grid.getOrElse(p, 0)
      if pValue > input then pValue
      else
        val nextGrid = p.getAdjacentPositions.filterNot(checked.contains).map{ adjacent =>
          (adjacent, grid.getOrElse(adjacent, 0) + grid.getOrElse(p, 0))
        }.foldLeft(grid)(_ + _)
        loop(positions.drop(1), nextGrid, checked :+ p)

    val initialGrid = Map(Position(0, 0) -> 1)
    loop(positions, initialGrid, Vector.empty[Position])

case class Position(x: Int, y: Int):
  def getAdjacentPositions: Vector[Position] =
    for {
      x <- (-1 to 1 by 1).toVector.map(_ + x)
      y <- (-1 to 1 by 1).toVector.map(_ + y) if (this.x, this.y) != (x, y)
    } yield Position(x, y)

def layerPositions(layer: Int): Vector[Position] = layer match
  case n if n < 0 => Vector.empty[Position]
  case 0 => Vector(Position(0,0))
  case l if l > 0 =>
    val right = (1-layer to layer by 1).toVector.map(Position(layer,_))
    val top = (layer-1 to -layer by -1).toVector.map(Position(_,layer))
    val left = (layer-1 to -layer by -1).toVector.map(Position(-layer,_))
    val bottom = (1-layer to layer by 1).toVector.map(Position(_,-layer))
    right ++ top ++ left ++ bottom

