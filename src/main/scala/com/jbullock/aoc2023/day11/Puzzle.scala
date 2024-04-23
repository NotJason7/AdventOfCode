package com.jbullock.aoc2023.day11

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2023/Day11/Input.txt").getLines.toSeq
  val galaxies =
    for
      y <- input.indices
      x <- input.head.indices
      if input(y)(x) == '#'
    yield Position(x, y)
  val expansionRows    = input.expansionRows
  val expansionColumns = input.expansionColumns
  val galaxyPairs: Seq[Set[Position]] =
    galaxies.flatMap(galaxy => galaxies.filterNot(_ == galaxy).map(other => Set(galaxy, other))).distinct
  def pairDistanceSum(expansionAmount: BigInt): BigInt = galaxyPairs
    .map(_.toSeq match
      case Seq(a, b) => a.expandedDistanceFrom(b, expansionAmount, expansionRows, expansionColumns)
    )
    .sum
  println(s"Part 1: ${pairDistanceSum(2)}")
  println(s"Part 2: ${pairDistanceSum(1000000)}")

extension (strings: Seq[String])
  def expansionRows: Seq[BigInt] = strings.zipWithIndex.flatMap { (s, i) =>
    if s.forall(_ == '.') then Some(i) else None
  }
  def expansionColumns: Seq[BigInt] = strings.transpose.map(_.mkString).expansionRows

case class Position(x: BigInt, y: BigInt):
  def distanceFrom(p: Position): BigInt = (x - p.x).abs + (y - p.y).abs
  def expandedDistanceFrom(
      p: Position,
      expansionAmount: BigInt,
      expansionRows: Seq[BigInt],
      expansionColumns: Seq[BigInt]
  ): BigInt =
    val expansionRowsBetween    = expansionRows.count(i => i.isBetween(y, p.y))
    val expansionColumnsBetween = expansionColumns.count(i => i.isBetween(x, p.x))
    val expansionsCrossed       = expansionRowsBetween + expansionColumnsBetween
    distanceFrom(p) + (expansionAmount * expansionsCrossed) - expansionsCrossed

extension (i: BigInt) def isBetween(a: BigInt, b: BigInt): Boolean = (a < i && i < b) || (b < i && i < a)
