package com.jbullock.aoc2017.day19

import scala.annotation.tailrec
import scala.io.Source

object Puzzle:
  @main def solve(): Unit =
    val input     = Source.fromResource("aoc/2017/Day19/Input.txt").getLines.toVector
    val widest    = input.map(_.length).max
    val processed = input.map(_.padTo(widest, ' '))
    val grid = for
      x <- processed.head.indices
      y <- processed.indices
      a = processed(y)(x)
      if a != ' '
    yield Position(x, y) -> a
    given Map[Position, Char] = grid.toMap
    val startPosition         = grid.toVector.filter((p, v) => p.y == 0 && v == '|').head._1
    val start                 = State(startPosition, South, Vector.empty[Position], Vector.empty[Char])
    val end                   = start.traverse
    println(s"Part 1: ${end.letters.mkString}")
    println(s"Part 2: ${end.visited.size}")

case class Position(x: Int, y: Int):
  def move(heading: Heading): Position = Position(x + heading.x, y + heading.y)

sealed trait Heading(val x: Int, val y: Int, val left: Heading, val right: Heading)
case object North extends Heading(0, -1, West, East)
case object South extends Heading(0, 1, East, West)
case object East  extends Heading(1, 0, North, South)
case object West  extends Heading(-1, 0, South, North)

case class State(position: Position, heading: Heading, visited: Vector[Position], letters: Vector[Char])(using
    grid: Map[Position, Char]
):
  def next: State =
    val current = grid(position)
    val effectiveHeading = current match
      case '+' => if grid.contains(position.move(heading.left)) then heading.left else heading.right
      case _   => heading
    val nextPosition = position.move(effectiveHeading)
    val nextVisited  = visited.appended(position)
    val nextLetters  = if Set('-', '|', '+').contains(current) then letters else letters.appended(current)
    State(nextPosition, effectiveHeading, nextVisited, nextLetters)
  @tailrec final def traverse: State =
    if grid.contains(position) then this.next.traverse
    else this
