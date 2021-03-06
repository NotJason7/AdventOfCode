package com.jbullock.aoc2021.day15

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.PriorityQueue

@main
def solvePuzzle(): Unit =
  val input: List[String] = Source
    .fromResource("aoc/2021/Day15/Input.txt")
    .getLines
    .toList
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
  val part2Answer = Puzzle.part2(input)
  println(s"Part2: $part2Answer")

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
    val validPoints = potentialPoints.filter(cavern.points.contains)
    validPoints
  def heuristic(using target: Point): Int =
    math.abs(p.x - target.x) + math.abs(p.y - target.y)
  def risk(using cavern: Cavern): Int =
    cavern.points(p)
  def score(using target: Point, cavern: Cavern): Int =
    cavern.points(p) + p.heuristic

case class Node(point: Point, parent: Option[Node], fitness: Int, risk: Int)
extension(n: Node)
  def adjacentNodes(using target: Point, cavern: Cavern): List[Node] =
    n.point.adjacentPoints.map{p =>
      Node(p, Some(n), n.risk + p.risk + p.heuristic, n.risk + p.risk)
    }
  def buildPath: Path =

    @tailrec
    def loop(list: List[Node]): List[Node] =
      val current = list.head
      current.parent match
        case None => list
        case Some(parent) => loop(list.prepended(parent))

    Path(loop(List(n)))

given Ordering[Node] with
  def compare(a: Node, b: Node): Int = b.fitness.compare(a.fitness)

case class Path(nodes: List[Node])
object Path:
  def fromNode(n: Node): Path =
    Path(List(n))
extension(p: Path)
  def extend(n: Node): Path =
    Path(p.nodes.prepended(n))
  def write(): Unit =
    println(p.nodes.map(n => s"(${n.point.x},${n.point.y}): ${n.risk}").mkString("\n"))



case class Cavern(points: Map[Point, Int], width: Int, height: Int)
extension(c: Cavern)
  def write(): Unit =
    val w = (0 to c.width).toList
    val h = (0 to c.height).toList
    val grid = h.map{ y =>
      w.map(x => Point(x, y))
    }.map(_.map(c.points(_)).mkString).mkString("\n")
    print(grid)
object Cavern:
  def fromList(l: List[String]): Cavern =
    val yRange = l.indices
    val width = l.head.length
    val xRange = l.head.indices
    val height = l.size
    val positions = l.map(_.toList.map(_.asDigit))
    val cavern = for {
      y <- l.indices
      x <- l.head.indices
    } yield Map(Point(x, y) -> positions(y)(x))
    Cavern(cavern.flatten.toMap, height - 1, width - 1)
  def fromListBigVersion(l: List[String]): Cavern =
    val yRange = l.indices
    val width = l.head.length
    val height = l.size
    val xRange = l.head.indices
    val positions = l.map(_.toList.map(_.asDigit))
    val cavern = for {
      xFactor <- (0 to 4).toList
      x <- l.head.indices
      yFactor <- (0 to 4).toList
      y <- l.indices
    } yield {
      val key = Point(x + (xFactor * width), y + (yFactor * height))
      val value = positions(y)(x) + (xFactor + yFactor)
      val trueValue = if value > 9 then value - 9 else value
      Map(key -> trueValue)
    }
    Cavern(cavern.flatten.toMap, height * 5 - 1, width * 5 - 1)



def aStar(start: Point)(using target: Point, cavern: Cavern): Path =

  @tailrec
  def loop(openList: mutable.PriorityQueue[Node], closedList: Set[Point])(using target: Point, cavern: Cavern): Path =

    val current = openList.dequeue()

    if current.point == target then
      current.buildPath
    else
      val newNodes = current.adjacentNodes.filterNot(node => closedList.contains(node.point))
      val overlap = openList.filter(existing =>
        newNodes.map(_.point).contains(existing.point)).toList
      val newOpenList = openList.filterNot(overlap.contains(_))
      val newOverlap = newNodes.filter(node => overlap.map(_.point).contains(node.point))
      val newUnseen = newNodes.filterNot(node => overlap.map(_.point).contains(node.point))
      newUnseen.foreach(newOpenList.enqueue(_))
      val bestOverlaps = overlap.map(oldNode =>
        val newNode = newOverlap.filter(_.point == oldNode.point).head
        if newNode.fitness < oldNode.fitness then newNode else oldNode
      )
      bestOverlaps.foreach(newOpenList.enqueue(_))

      val newClosedList = closedList + current.point
      loop(newOpenList, newClosedList)

  val startNode = mutable.PriorityQueue[Node](Node(start, None, start.heuristic, 0))

  loop(startNode, Set.empty)

object Puzzle:

  def part1(input: List[String]): Int =
    given cavern: Cavern = Cavern.fromList(input)
    given target: Point = Point(cavern.width, cavern.height)
    val start = Point(0, 0)
    val path = aStar(start)
    val risk = path.nodes.last.risk
    risk

  def part2(input: List[String]): Int =
    given cavern: Cavern = Cavern.fromListBigVersion(input)
    given target: Point = Point(cavern.width, cavern.height)
    val start = Point(0, 0)
    val path = aStar(start)
    val risk = path.nodes.last.risk
    risk




