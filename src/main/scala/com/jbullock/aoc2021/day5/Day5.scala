package com.jbullock.aoc2021.day5

import scala.io.Source

@main
def runDay5(): Unit =
  println(s"Part one: ${Day5.part1}")
  // println(s"Part two: ${Day5.part2}")

object Day5 {
  val input: List[Vector] = Source
    .fromResource("2021/Day5Input.txt")
    // .fromResource("2021/Day5TestInput.txt")
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
    // println(filtered)
    val occupied = getOccupied(filtered)
    val overlaps = getOverlaps(occupied)
    val overlapsGreaterThanOne = overlaps.collect{
      case (coordinate, overlapSize) if overlapSize > 1 =>
        (coordinate, overlapSize)
    }
    overlapsGreaterThanOne.keySet.size

  // def part2: Int = ???

  def getOccupied(vectors: List[Vector]): List[Coordinate] =
    vectors.flatMap { vector =>
      if (vector.isHorizontal) {
        val y = vector.to.y
        val minX = vector.from.x min vector.to.x
        val maxX = vector.from.x max vector.to.x
        (minX to maxX)
          .toList
          .map(x => Coordinate(x, y))
      } else if (vector.isVertical) {
        val x = vector.to.x
        val minY = vector.from.y min vector.to.y
        val maxY = vector.from.y max vector.to.y
        (minY to maxY)
          .toList
          .map(y => Coordinate(x, y))
      } else {
        None
      }
    }
  
  def getOverlaps(occupied: List[Coordinate]): Map[Coordinate, Int] =
    occupied.groupMapReduce(identity)(_ => 1)(_ + _)

}

  // val testInput = List(
  //   Line(Coordinate(0, 9),Coordinate(5, 9)),
  //   Line(Coordinate(8, 0),Coordinate(0, 8)),
  //   Line(Coordinate(9, 4),Coordinate(3, 4)),
  //   Line(Coordinate(2, 2),Coordinate(2, 1)),
  //   Line(Coordinate(7, 0),Coordinate(7, 4)),
  //   Line(Coordinate(6, 4),Coordinate(2, 0)),
  //   Line(Coordinate(0, 9),Coordinate(2, 9)),
  //   Line(Coordinate(3, 4),Coordinate(1, 4)),
  //   Line(Coordinate(0, 0),Coordinate(8, 8)),
  //   Line(Coordinate(5, 5),Coordinate(8, 2))
  // )

  // val filteredTestInput = testInput.filter(!_.isDiagnoal)

  // val expandedfilteredInput = filteredTestInput.toList.map { (k, v) =>
  //   if k._1 == v._1 then
  //     Range(List(k._2, v._2).min, List(k._2, v._2).max + 1).toList.map((k._1, _))
  //   else
  //     Range(List(k._1, v._1).min, List(k._1, v._1).max + 1).toList.map((_, k._2))
  // }

  // def expand(input: Map[(Int, Int), (Int, Int)]): List[(Int, Int)] =
  //   val endPoints = input.toList.map { (k, v) =>
  //     val expanded = if k._1 == v._1 then
  //       val range = List(k._2, v._2)
  //       Range(range.min, range.max + 1).toList.flatMap((k._1, _))
  //     else
  //       val range = List(k._1, v._1)
  //       Range(range.min, range.max + 1).toList.flatMap((_, k._2))
  //   }
