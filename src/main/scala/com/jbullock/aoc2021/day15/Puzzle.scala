package com.jbullock.aoc2021.day15

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: List[String] = Source
    .fromResource("2021/Day15/Example.txt")
    .getLines
    .toList
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
//  val part2Answer = Puzzle.part2(input)
//  println(s"Part2: $part2Answer")

case class Point(x: Int, y: Int)
extension(p: Point)
  def adjacentPoints(using cavern: Cavern): List[Point] =
    val transformations = List(
      (-1, 0),
      (0, -1),
      (1, 0),
      (0, 1)
    )
    val potentialPoints = transformations.map(t => Point(t._1 + p.x, t._2 + p.y))
    val validPoints = potentialPoints.filter(cavern.positions.contains)
    validPoints
  def manhattanDistance(target: Point): Int =
    math.abs(p.x - t.x) + math.abs(p.y - t.y)
  def risk(using cavern: Cavern): Int =
    cavern.positions(p)

case class Node(point: Point, path: Path)
extension(n: Node)
  def risk: Int =
    n.path.risk
  def extensions: List[Node] =
    n.point.adjacentPoints.map(p => Node(p, n.path.extend(p)))


case class Cavern(positions: Map[Point, Int])
object Cavern:
  def fromList(l: List[String]): Cavern =
    val yRange = l.indices
    val xRange = l.head.indices
    val positions = l.map(_.toList.map(_.asDigit))
    val cavern = for {
      y <- l.indices
      x <- l.head.indices
    } yield Map(Point(x, y) -> positions(y)(x))
    Cavern(cavern.flatten.toMap)

case class Path(visited: List[Point], risk: Int)
object Path:
  def fromPoint(p: Point)(using cavern: Cavern): Path =
    Path(List(p), p.risk)
extension(p: Path)
  def extend(point: Point)(using cavern: Cavern): Path =
    val currentRisk = p.risk
    val pointRisk = point.risk
    println(s"Next risk is ${currentRisk+pointRisk} ( current is $currentRisk and point is $pointRisk)")
    Path(point :: p.visited, point.risk + point.risk)

object Puzzle:

  def part1(input: List[String]): Int =
    given cavern: Cavern = Cavern.fromList(input)
    val start = Point(0, 0)
    val end = Point(input.head.length-1, input.size-1)
    val path = pathFind(start, end)
    println(path)
    path.risk

  def part2(input: List[String]): Int = ???

  def searchNode(currentNode: Node, visitedNodes: List[Node], target: Point)(using cavern: Cavern): Path =


//  def buildPaths(current: Path, target: Point)(using cavern: Cavern): List[Path] =
//    val head = current.visited.head
//    val nextSteps = head.adjacentPoints
//    if nextSteps.contains(target) then
//      List(current.extend(target))
//    nextSteps.collect{ case point: Point if !current.visited.contains(point) =>
//      current.extend(point)
//    }
//
//  def pathFind(start: Point, target: Point)(using cavern: Cavern): Path =
//
//    @tailrec
//    def loop(sortedPaths: List[Path], target: Point): Path =
//      val completePaths = sortedPaths.filter(_.visited.contains(target))
//      if completePaths.nonEmpty then completePaths.head
//      else
//        val nextPaths = sortedPaths.flatMap(p => buildPaths(p, target))
//        val sortedNextPaths = nextPaths.sortWith(_.risk > _.risk)
//        loop(sortedNextPaths, target)
//
//    val startPath = List(Path.fromPoint(start))
//    loop(startPath, target)
