package com.jbullock.aoc2021.day05

import scala.io.Source

@main
def runDay5(): Unit =
  println(s"Part one: ${Day5.part1}")
  println(s"Part two: ${Day5.part2}")

object Day5 {
  val input: List[Vector] = Source
    .fromResource("aoc/2021/Day05/Day5Input.txt")
    .getLines
    .toList
    .map(_.replace(" -> ", ","))
    .map(_.split(',').toList.map(_.toInt))
    .map{ line =>
      val from = Coordinate(line(0), line(1))
      val to = Coordinate(line(2), line(3))
      Vector(from, to)
    }

  def part1: Int = 
    val filtered = input.filter(vector => vector.isHorizontal || vector.isVertical)
    val occupied = getOccupied(filtered)
    val overlaps = getOverlaps(occupied)
    val overlapsGreaterThanOne = overlaps.collect{
      case (coordinate, overlapSize) if overlapSize > 1 =>
        (coordinate, overlapSize)
    }
    overlapsGreaterThanOne.keySet.size

  def part2: Int = 
    val occupied = getOccupied(input)
    val overlaps = getOverlaps(occupied)
    val overlapsGreaterThanOne = overlaps.collect{
      case (coordinate, overlapSize) if overlapSize > 1 =>
        (coordinate, overlapSize)
    }
    overlapsGreaterThanOne.keySet.size

  def getOccupied(vectors: List[Vector]): List[Coordinate] =
    vectors.flatMap { vector =>
      if (vector.isHorizontal) {
        val xDirection = if vector.isLeftToRight then 1 else -1
        (vector.from.x to vector.to.x by xDirection)
          .toList
          .map(x => Coordinate(x, vector.to.y))
      } else if (vector.isVertical) {
        val yDirection = if vector.isBottomToTop then 1 else -1
        (vector.from.y to vector.to.y by yDirection)
          .toList
          .map(y => Coordinate(vector.to.x, y))
      } else {
        val xDirection = if vector.isLeftToRight then 1 else -1
        val yDirection = if vector.isBottomToTop then 1 else -1
        val xCoordinates = (vector.from.x to vector.to.x by xDirection).toList
        val yCoordinates = (vector.from.y to vector.to.y by yDirection).toList
        (xCoordinates zip yCoordinates).map{
          case (x: Int, y: Int) => Coordinate(x, y)
        }
      }
    }
  
  def getOverlaps(occupied: List[Coordinate]): Map[Coordinate, Int] =
    occupied.groupMapReduce(identity)(_ => 1)(_ + _)

}
