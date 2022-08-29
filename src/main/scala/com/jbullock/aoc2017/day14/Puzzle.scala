package com.jbullock.aoc2017.day14

import com.jbullock.aoc2017.day10.Puzzle.part2 as knotHash

import scala.annotation.tailrec


object Puzzle:
  @main
  def solve(): Unit =
//    val key = "ugkiagan"
    val key = "flqrgnkx"
    val rows = (0 to 127).toVector.map(n => s"$key-$n")
    val hashed = rows.map(knotHash).map(h => BigInt(h, 16).toString(2).reverse.padTo(128,0).reverse.mkString)
    val used = hashed.map(_.count(_ == '1')).sum
    println(s"Part 1: $used")

    val map = (for {
      y <- (0 to 127)
      x <- (0 to 127)
      h = hashed(y)(x)
    } yield Position(x, y) -> h).toMap.filter((k, v) => v == '1')
    println(map)

case class Position(x: Int, y: Int):
  def adjacent: Vector[Position] = Vector(
    Position(x-1, y), Position(x+1, y),
    Position(x, y-1), Position(x, y+1)
  ).filter(p => p.x >= 0 && p.x <= 127 && p.y >= 0 && p.y <= 127)

//def searchMap(map: Map[Position -> String]): Int =
//  @tailrec
//  def loop(toSearch: Vector[Position], regions: Set[Set[Position]]): Int =
//    if toSearch.isEmpty then regions.size else
//      @tailrec
//      def innerLoop(positions: Vector[Position], region: Set[Position]): Set[Position] =
//        if positions.isEmpty then region else
//          val next = positions.head
