package com.jbullock.aoc2022.day14

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2022/Day14/Input.txt").getLines.toVector
  val start = Cave.fromStrings(input)
  val part1 = start.dropSandUntilFreeFall(false).sandCount
  println(s"Part 1: $part1")
  val startWithFloor = start.addFloorAt(2)
  val end            = startWithFloor.dropSandUntilFreeFall(true)
  val part2 = end.sandCount
  println(s"Part 2: $part2")

sealed trait Space(val symbol: Char)
case object Air  extends Space('.')
case object Rock extends Space('#')
case object Sand extends Space('o')

case class Position(x: Int, y: Int):
  def next: Vector[Position] = Vector((0, 1), (-1, 1), (1, 1)).map { case (dx, dy) => Position(x + dx, y + dy) }
  def to(p: Position): Vector[Position] = ((p.x - x).sign, (p.y - y).sign) match
    case (dx, _) if dx != 0 => (x to p.x by dx).map(Position(_, y)).toVector
    case (_, dy) if dy != 0 => (y to p.y by dy).map(Position(x, _)).toVector
    case _ => throw RuntimeException(s"Unable to calculate positions between ($x,$y) and (${p.x},${p.y}).")

case class Cave(terrain: Map[Position, Space]):
  val (yMin, yMax)   = (terrain.keys.map(_.y).min, terrain.keys.map(_.y).max)
  val sandCount: Int = terrain.values.count(_ == Sand)
  val (xMin, xMax)   = (terrain.keys.map(_.x).min, terrain.keys.map(_.x).max)
  def show(): Unit =
    val cave = (for
      y <- (yMin to yMax)
      x <- (xMin to xMax)
    yield terrain.getOrElse(Position(x, y), Air).symbol + " ").grouped(xMax - xMin + 1).toVector.map(_.mkString)
    cave.foreach(println)
  def addFloorAt(y: Int): Cave =
    val floorMin = 500 - (yMax + 2)
    val floorMax = 500 + (yMax + 2)
    val badFloor = (floorMin to floorMax).map(Position(_, yMax + y) -> Rock).toMap[Position, Space]
    this.copy(terrain = terrain ++ badFloor)
  private def dropSand(withFloor: Boolean): Cave =
    @tailrec def loop(current: Position): Option[Position] =
      if !withFloor && current.y >= yMax then None
      else
        current.next.find(terrain.getOrElse(_, Air) == Air) match
          case Some(position) => loop(position)
          case None           => Some(current)
    loop(Position(500, 0)) match
      case Some(sand) => this.copy(terrain = terrain + (sand -> Sand))
      case None       => this
  def dropSandUntilFreeFall(withFloor: Boolean): Cave =
    @tailrec def loop(cave: Cave): Cave =
      if withFloor && cave.terrain.isDefinedAt(Position(500, 0)) then cave
      else
        val next = cave.dropSand(withFloor)
        if next == cave then cave
        else loop(next)
    loop(this)

object Cave:
  def fromStrings(ss: Vector[String]): Cave = Cave(
    ss
      .flatMap { line =>
        val positions = line.split(" -> ").toVector.map(_.split(",").toVector.map(_.toInt)).map { case Vector(x, y) =>
          Position(x, y)
        }
        val start = positions.head
        val rest  = positions.tail
        rest.foldLeft(Vector(start)) { case (acc, position) => acc ++ acc.last.to(position) }
      }
      .distinct
      .map(p => (p -> Rock))
      .toMap
  )
