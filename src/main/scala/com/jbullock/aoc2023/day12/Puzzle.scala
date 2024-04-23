package com.jbullock.aoc2023.day12

import scala.util.matching.Regex

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2023/Day12/Sample.txt").getLines.toSeq
  val rows  = input.flatMap(s => Row.fromString(s))
  val part1 = rows.map(_.possibleArrangement.size).sum
  println(s"Part 1: $part1")
  println(rows.head)
  println(rows.head.unfold(5))

extension [A](s: Seq[A]) def repeat(i: Int): Seq[A] = (1 to i).flatMap(_ => s)

case class Row(springs: String, damaged: Seq[Int]):
  def unfold(i: Int): Row  = Row(Seq(springs).repeat(i).mkString("?"), damaged.repeat(i))
  val damagedRegex: String = damaged.map(i => "#" * i).mkString("\\.*", "\\.+", "\\.*")
  def possibleSprings: Vector[String] = springs.foldLeft(Vector(""))((springs, c) =>
    c match
      case '?' => springs.map(_ :+ '.') ++ springs.map(_ :+ '#')
      case _   => springs.map(_ :+ c)
  )
  def possibleArrangement: Vector[String] = possibleSprings.filter(s => s.matches(damagedRegex))

object Row:
  def fromString(s: String): Option[Row] = s match
    case s"$springs $damaged" =>
      Some(Row(springs, damaged.split(',').toSeq.map(_.toInt)))
    case _ => None
