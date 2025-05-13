package com.jbullock.aoc2024.day12

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2024/Day12/Sample4.txt").getLines().toSeq
  val points =
    for
      y <- input.indices
      x <- input.head.indices
    yield Point(x, y)
  val map     = points.map(p => p -> input(p.y)(p.x)).toMap
  val regions = findRegions(map, points)
//  val part1 = regions.map(_.price(map)).sum
//  println(s"Part 1: $part1")
  val c = regions.filter(_.name == 'E').head
  println(c.countSides)

def findRegions(map: Map[Point, Char], points: Seq[Point]): Seq[Region] =
//  val startingPoints = map.keys.toSeq.sortWith((a, b) => a.y < b.y).sortWith((a, b) => a.x < b.x)

  @tailrec
  def loop(startingPoints: Seq[Point], regionsFound: Seq[Region] = Seq.empty[Region]): Seq[Region] =
    startingPoints.headOption match
      case None => regionsFound
      case Some(nextStartingPoint) =>
        val key                = map.getOrElse(nextStartingPoint, ' ')
        val region             = findRegion(nextStartingPoint, key, map)
        val nextStartingPoints = startingPoints.filterNot(region.points.contains)
        val nextRegionsFound   = regionsFound :+ region
        loop(nextStartingPoints, nextRegionsFound)
  loop(points)

def findRegion(startingPoint: Point, key: Char, map: Map[Point, Char]): Region =

  @tailrec def loop(toSearch: Seq[Point], found: Set[Point]): Region =
    toSearch.headOption match
      case None => Region(key, found)
      case Some(nextPoint) =>
        val newPoints =
          nextPoint.adjacentPoints
            .filter(adjacentPoint => map.getOrElse(adjacentPoint, ' ') == key)
            .filterNot(point => found.contains(point) || toSearch.contains(point))
        val nextToSearch = toSearch.tail ++ newPoints
        val nextFound    = found + nextPoint
        loop(toSearch.tail ++ newPoints, found + nextPoint)

  loop(Seq(startingPoint), Set.empty[Point])

case class Point(x: Int, y: Int):
  def move(direction: Direction): Point = Point(x + direction.dx, y + direction.dy)
  def adjacentPoints: Seq[Point]        = Direction.allDirections.map(this.move).toSeq

sealed trait Direction(val dx: Int, val dy: Int)
case object Up    extends Direction(0, -1)
case object Down  extends Direction(0, 1)
case object Left  extends Direction(-1, 0)
case object Right extends Direction(1, 0)
object Direction:
  val allDirections: Set[Direction] = Set(Up, Down, Left, Right)

case class Region(name: Char, points: Set[Point]):
  val area: Int = points.size
  def perimeter(map: Map[Point, Char]): Int =
    points.toSeq.flatMap(_.adjacentPoints).count(!points.contains(_))
  def price(map: Map[Point, Char]): Int = area * perimeter(map)
  def countSides: Int =
    val borderPoints = points.flatMap(p => p.adjacentPoints.filterNot(points.contains)).toSeq

    @tailrec
    def buildEdge(currentEdge: Seq[Point], potentialPoints: Seq[Point], isHorizontal: Boolean): Seq[Point] =
      val backDirection    = if isHorizontal then Left else Up
      val forwardDirection = if isHorizontal then Right else Down
      val back             = Seq(currentEdge.head.move(backDirection)).filter(potentialPoints.contains)
      val forward          = Seq(currentEdge.last.move(forwardDirection)).filter(potentialPoints.contains)
      if back == forward then currentEdge
      else
        val nextEdge = back ++ currentEdge ++ forward
        buildEdge(nextEdge, potentialPoints, isHorizontal)

    @tailrec def findAllEdges(
        borders: Seq[Point],
        edges: Set[Set[Point]] = Set.empty[Set[Point]]
    ): Set[Set[Point]] =
      borders.headOption match
        case None => edges
        case Some(borderStart) =>
          val horizontal  = buildEdge(Seq(borderStart), borders, true).toSet
          val vertical    = buildEdge(Seq(borderStart), borders, false).toSet
          val longestEdge = Seq(horizontal, vertical).maxBy(_.size)
          findAllEdges(borders.tail, edges + longestEdge)

    val edges = findAllEdges(borderPoints)

    val sides = edges.map(edge =>
      println(s"Considering $edge")
      val directionCount = Direction.allDirections.count(direction =>
        val potentialRegionPoints = edge.toSeq.flatMap(_.adjacentPoints)
        potentialRegionPoints.exists(points.contains)
      )
      println(s"Edge $edge borders in $directionCount direction(s)")
      directionCount
    )

    sides.sum
