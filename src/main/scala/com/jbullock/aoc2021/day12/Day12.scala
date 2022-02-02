package com.jbullock.aoc2021.day12

import scala.annotation.tailrec
import scala.io.Source

@main
def runDay12(): Unit =
  Day12.part1()
//  Day12.part2()

case class Path(start: String, end: String)


object Day12:
  val cave = Source
    .fromResource("2021/Day12/Example01.txt")
    .getLines
    .toList
    .map(_.split('-').toList.toString)
    .foldLeft(Map.empty) {
      case (map, list) => map + (list(0) -> list(1)) + (list(1) -> list(0))
    }
//    .toMap[String, String]
//    .flatMap(ss => Map(ss(0) -> ss(1)))
//    .foldLeft(Map[String,String]())(_.++(_))


  def part1(): Unit =
    println(cave)
//    val caveMap = cave.toMap
//    println(caveMap)