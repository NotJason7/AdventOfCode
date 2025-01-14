package com.jbullock.aoc2024.day04

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2024/Day04/Input.txt").getLines.toSeq
  val wordSearch = (for
    y <- input.indices
    x <- input.head.indices
  yield Position(x, y) -> input(y)(x)).toMap
  val target = "XMAS"
  val part1  = wordSearch.countOccurrences(target)
  println(s"Part 1: $part1")
  val part2 = wordSearch.countOccurrencesPart2
  println(s"Part 2: $part2")

extension (m: Map[Position, Char])
  def countOccurrencesPart2: Int =
    val startingPositions = m.filter(_._2 == 'A').keys.toSeq
    val occurrences = for
      start <- startingPositions
      occurrence = Direction.crossSearch(m, start, "MS")
      if occurrence
    yield 1
    occurrences.sum

  def countOccurrences(s: String): Int =
    val startingPositions = m.filter(_._2 == s.head).keys.toSeq
    val occurrences = for
      start     <- startingPositions
      direction <- Direction.values.toSeq
      occurrence = m.searchFromPosition(start, direction, s.tail)
      if occurrence
    yield 1
    occurrences.sum

  @tailrec def searchFromPosition(p: Position, d: Direction, remainingChars: String): Boolean =
    if remainingChars.isEmpty then true
    else
      val nextPosition = p.move(d)
      val nextChar     = m.getOrElse(nextPosition, ' ')
      if nextChar == remainingChars.head then searchFromPosition(nextPosition, d, remainingChars.tail)
      else false

enum Direction(val x: Int, val y: Int):
  case Up extends Direction(0, -1)
  case Down extends Direction(0, 1)
  case Left extends Direction(-1, 0)
  case Right extends Direction(1, 0)
  case UpLeft extends Direction(-1, -1)
  case UpRight extends Direction(1, -1)
  case DownLeft extends Direction(-1, 1)
  case DownRight extends Direction(1, 1)
object Direction:
  def crossSearch(m: Map[Position, Char], p: Position, target: String): Boolean =
    val targets = Seq(target, target.reverse)
    val nw      = m.getOrElse(p.move(Direction.UpRight), ' ')
    val sw      = m.getOrElse(p.move(Direction.DownRight), ' ')
    val se      = m.getOrElse(p.move(Direction.DownLeft), ' ')
    val ne      = m.getOrElse(p.move(Direction.UpLeft), ' ')
    val found   = Seq(s"$ne$sw", s"$nw$se")
    found.forall(targets.contains)

case class Position(x: Int, y: Int):
  def move(d: Direction): Position = Position(x + d.x, y + d.y)
