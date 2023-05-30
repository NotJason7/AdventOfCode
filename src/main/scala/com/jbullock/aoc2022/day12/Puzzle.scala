package com.jbullock.aoc2022.day12

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2022/Day12/Input.txt").getLines.toVector
  given heightMap: Map[Position, Int] = (for
    y <- input.indices
    x <- input.head.indices
    height = input(y).replace('S', 'a').replace('E', 'z')(x) - 'a'.toInt + 1
  yield Position(x, y) -> height).toMap
  val start           = Position(input.transpose.indexWhere(_.contains('S')), input.indexWhere(_.contains('S')))
  given end: Position = Position(input.transpose.indexWhere(_.contains('E')), input.indexWhere(_.contains('E')))
  val part1           = aStar(start).get.size
  println(s"Part 1: $part1")
  val part2 = heightMap.filter(_._2 == 1).keys.toVector.flatMap(aStar).map(_.size).min
  println(s"Part 2: $part2")

sealed trait Direction(val x: Int, val y: Int)
case object Up    extends Direction(0, -1)
case object Down  extends Direction(0, 1)
case object Left  extends Direction(-1, 0)
case object Right extends Direction(1, 0)

case class Position(x: Int, y: Int):
  def move(d: Direction): Position = Position(x + d.x, y + d.y)
  def distance(p: Position): Int   = (x - p.x).abs + (y - p.y).abs
  def adjacent(using heightMap: Map[Position, Int]): Set[Position] =
    Set(Up, Down, Left, Right).map(move).filter(p => heightMap.contains(p) && heightMap(p) - heightMap(this) <= 1)

case class Node(position: Position, parent: Option[Node], pathLength: Int, distance: Int):
  val fitness: Int = pathLength + distance
  def adjacent(using target: Position, heightMap: Map[Position, Int]): Set[Node] =
    position.adjacent.map(p => Node(p, Some(this), pathLength + 1, p.distance(target)))
  def totalPath: Vector[Position] = Vector.unfold(this)(node => node.parent.map(parent => (node.position, parent)))

def aStar(start: Position)(using target: Position, heightMap: Map[Position, Int]): Option[Vector[Position]] =

  @tailrec def loop(openNodes: Vector[Node], closedPositions: Set[Position])(using
      target: Position,
      heightMap: Map[Position, Int]
  ): Option[Vector[Position]] = openNodes.headOption match
    case Some(node) if node.position == target => Some(node.totalPath)
    case Some(current: Node) =>
      val validNodes      = current.adjacent.toVector.filterNot(node => closedPositions.contains(node.position))
      val (seen, unseen)  = validNodes.partition(node => openNodes.tail.map(_.position).contains(node.position))
      val (oldSeen, open) = openNodes.tail.partition(node => seen.map(_.position).contains(node.position))
      val updatedSeen = oldSeen.map { before =>
        val after = seen.filter(_.position == before.position).head
        if after.fitness < before.fitness then after else before
      }
      val nextOpen = (open ++ unseen ++ updatedSeen).sortWith(_.fitness < _.fitness)
      loop(nextOpen, closedPositions + current.position)
    case _ => None

  loop(Vector(Node(start, None, 0, start.distance(target))), Set.empty)
