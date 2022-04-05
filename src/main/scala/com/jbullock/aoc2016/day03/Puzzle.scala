package com.jbullock.aoc2016.day03

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: List[String] = Source
    .fromResource("aoc/2016/Day03/Input.txt")
    .getLines
    .toList
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
  val part2Answer = Puzzle.part2(input)
  println(s"Part2: $part2Answer")

// Triangle w/Sides: a, b, c
// a + b > c &&
// a + c > b &&
// b + c > a
case class Triangle(a: Int, b: Int, c: Int)
extension (t: Triangle)
  def isValidTriangle: Boolean =
    (t.a + t.b > t.c) && (t.a + t.c > t.b) && (t.b + t.c > t.a)
object Triangle:
  def fromList(l: List[Int]): Triangle =
    if l.size != 3 then
      throw new Exception(s"Not the right number of sides for a triangle, found ${l.size}, expected 3.")
    else
      Triangle(l(0), l(1), l(2))

extension(s: String)
  def toTriangleNumbers: List[Int] =
    // This is absolutely not safe because not all strings can become triangles
    s.trim.split("\\s+").toList.map(_.toInt)

object Puzzle:

  def part1(input: List[String]): Int =
    val triangles = input.map(_.toTriangleNumbers).map(Triangle.fromList)
    triangles.count(_.isValidTriangle)

  def part2(input: List[String]): Int =
    val triangles = input.map(_.toTriangleNumbers)
      .transpose
      .flatten
      .sliding(3,3)
      .toList
      .map(Triangle.fromList)
    triangles.count(_.isValidTriangle)
