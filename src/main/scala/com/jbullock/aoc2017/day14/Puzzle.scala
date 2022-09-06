package com.jbullock.aoc2017.day14

import com.jbullock.aoc2017.day10.Puzzle.part2 as knotHash

import scala.annotation.tailrec


object Puzzle:
  @main
  def solve(): Unit =
    val key = "ugkiagan"
//    val key = "flqrgnkx"
    val rows = (0 to 127).toVector.map(n => s"$key-$n")
    val hashed = rows.map(knotHash).map(h => BigInt(h, 16).toString(2).reverse.padTo(128,0).reverse.mkString)
    val used = hashed.map(_.count(_ == '1')).sum
    println(s"Part 1: $used")

//    val map = (for {
//      y <- (0 to 127)
//      x <- (0 to 127)
//    } yield Position(x, y)).toVector.filter(p => hashed(p.y)(p.x) == '1')
//    val regions = findRegions(map)
//    println(regions.size)

case class Position(x: Int, y: Int):
  def adjacent: Vector[Position] = Vector(
    Position(x-1, y), Position(x+1, y),
    Position(x, y-1), Position(x, y+1)
  ).filter(p => p.x >= 0 && p.x <= 127 && p.y >= 0 && p.y <= 127)

//case class Region(positions: Set[Position])
//object Region:
//  def fromPosition(p: Position, valid: Vector[Position]): Region =
//    @tailrec
//    def loop(toSearch: Vector[Position], region: Set[Position]): Region =
//      if toSearch.isEmpty then Region(region) else
//        val next = toSearch.head
//        val newInRegion = next.adjacent.filter(valid.contains).filterNot(region.contains)
//        val positionsToSearch = newInRegion ++ toSearch.tail
//        val totalRegion = region ++ newInRegion.toSet
//        loop(positionsToSearch, totalRegion)
//    loop(Vector(p), Set(p))
//
//def findRegions(positions: Vector[Position]): Set[Region] =
//  @tailrec
//  def loop(toSearch: Vector[Position], regions: Set[Region]): Set[Region] =
//    if toSearch.isEmpty then regions else
//      val nextPosition = toSearch.head
//      val region = Region.fromPosition(nextPosition, positions)
//      val totalRegions = regions + region
//      val containedPositions = regions.map(_.positions).flatten
//      val leftToSearch = toSearch.filterNot(containedPositions.contains)
//      loop(leftToSearch, totalRegions)
//  loop(positions, Set.empty[Set[Position]])
