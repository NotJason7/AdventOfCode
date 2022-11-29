package com.jbullock.aoc2017.day14

import com.jbullock.aoc2017.day10.Puzzle.part2 as knotHash

import scala.annotation.tailrec

object Puzzle:
  @main
  def solve(): Unit =
    val key    = "ugkiagan"
    val rows   = (0 to 127).toVector.map(n => s"$key-$n")
    val hashed = rows.map(knotHash).map(_.hexToBinary)
    val used   = hashed.map(_.count(_ == '1')).sum
    println(s"Part 1: $used")

    val grid = (for {
      y <- (0 to 127)
      x <- (0 to 127)
    } yield Position(x, y)).toSet.filter(p => hashed(p.y)(p.x) == '1')
    val regions = findRegions(grid)
    println(s"Found ${regions.size} regions, should be 1242")

extension (s: String)
  def hexToBinary: String = s.map(c => ("000" + BigInt(s"$c", 16).toString(2)).takeRight(4)).mkString

case class Position(x: Int, y: Int):
  def adjacent: Set[Position] = Set(
    Position(x - 1, y),
    Position(x + 1, y),
    Position(x, y - 1),
    Position(x, y + 1)
  ).filter(p => p.x >= 0 && p.x <= 127 && p.y >= 0 && p.y <= 127)

case class Region(positions: Set[Position])
object Region:
  def fromValidPosition(p: Position, valid: Set[Position]): Region =
    @tailrec
    def loop(toSearch: Set[Position], region: Set[Position]): Region =
      if toSearch.isEmpty then Region(region)
      else
        val nextPosition             = toSearch.head
        val nextRegion               = region + nextPosition
        val validAdjacentNotInRegion = nextPosition.adjacent.diff(nextRegion).intersect(valid)
        val nextToSearch             = toSearch - nextPosition ++ validAdjacentNotInRegion
        loop(nextToSearch, nextRegion)
    loop(Set(p), Set.empty[Position])

def findRegions(positions: Set[Position]): Set[Region] =
  @tailrec
  def loop(toSearch: Set[Position], regions: Set[Region]): Set[Region] =
    if toSearch.isEmpty then regions
    else
      val region       = Region.fromValidPosition(toSearch.head, positions)
      val leftToSearch = toSearch.diff(region.positions)
      val totalRegions = regions + region
      loop(leftToSearch, totalRegions)
  loop(positions, Set.empty[Region])
