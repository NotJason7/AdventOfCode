package com.jbullock.aoc2024.day10

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2024/Day10/Input.txt").getLines.toSeq
  val map = (for
    y <- input.indices
    x <- input.head.indices
  yield Point(x, y) -> input(y)(x).asDigit).toMap
  val trailHeads: Seq[Point] = map.filter((_, height) => height == 0).toSeq.map(_._1)
  val trailsByTrailHeads     = trailHeads.map(trailHead => trailHead -> findTrails(trailHead, map))
  val trailHeadScores        = trailsByTrailHeads.map { case (trailHead, trails) => trails.map(_.last).distinct.size }
  println(s"Part 1: ${trailHeadScores.sum}")
  val trailHeadRatings = trailsByTrailHeads.map { case (trailHead, trails) => trails.size }
  println(s"Part 2: ${trailHeadRatings.sum}")

def findTrails(trailHead: Point, map: Map[Point, Int]): Seq[Seq[(Point, Int)]] =

  def loop(currentTrails: Seq[Seq[(Point, Int)]], currentHeight: Int): Seq[Seq[(Point, Int)]] =
    if currentHeight == 9 then currentTrails
    else
      currentTrails.flatMap { currentTrail =>
        val current = currentTrail.last
        val adjacentPoints = Direction.allDirections
          .map(current._1.move)
          .map(p => (p, map.getOrElse(p, -1)))
        val validExtensions = adjacentPoints.filter(_._2 == currentHeight + 1)
        if validExtensions.isEmpty then Seq.empty[Seq[(Point, Int)]]
        else
          val nextTrails = validExtensions.map(extension => currentTrail :+ extension)
          loop(nextTrails, currentHeight + 1)
      }
  loop(Seq(Seq((trailHead, 0))), 0)

case class Point(x: Int, y: Int):
  def move(d: Direction): Point = Point(x + d.x, y + d.y)
  override def toString: String = s"P($x,$y)"

sealed trait Direction(val x: Int, val y: Int)
object Direction:
  case object Up    extends Direction(0, -1)
  case object Down  extends Direction(0, 1)
  case object Left  extends Direction(-1, 0)
  case object Right extends Direction(1, 0)
  val allDirections: Seq[Direction] = Seq(Up, Down, Left, Right)
