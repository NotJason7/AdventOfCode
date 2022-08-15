package com.jbullock.aoc2017.day10

import scala.io.Source

@main
def solvePuzzle(): Unit =
  println(s"Part 1: ${Puzzle.part1}")
  println(s"Part 2: ${Puzzle.part2}")

object Puzzle:
  val input: String = Source.fromResource("aoc/2017/Day10/Input.txt").getLines.toVector.head
  val start: State = State(Knot((0 to 255).toVector), 0, 0)
  def part1: Int =
    val twists = input.split(",").toVector.map(_.toInt)
    val end = twists.foldLeft(start)((s, v) => s.next(v))
    end.knot.values.take(2).product
  def part2: String =
    val twists = input.map(_.toInt).toVector ++ Vector(17,31,73,47,23)
    val repeated = Vector.fill(64)(twists).flatten
    val sparse = repeated.foldLeft(start)((s, v) => s.next(v)).knot.values
    val dense = sparse.grouped(16).toVector.map(_.reduce(_ ^ _))
    dense.map(_.toHexString).mkString

case class State(knot: Knot, index: Int, skip: Int):
  def next(size: Int): State = State(knot.twist(index, size), (index + size + skip) % knot.values.length, skip+1)

case class Knot(values: Vector[Int]):
  def twist(current: Int, size: Int): Knot =
    val start = current % values.length
    val shift = values.takeRight(values.length-start) ++ values.take(start)
    val twist = shift.slice(0, size).reverse ++ shift.takeRight(values.length-size)
    val unshift = twist.takeRight(start) ++ twist.take(values.length-start)
    Knot(unshift)
