package com.jbullock.aoc2016.day07

import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: Vector[String] = Source.fromResource("aoc/2016/Day07/Input.txt").getLines.toVector
  println(s"Part1: ${Puzzle.part1(input)}")
  println(s"Part2: ${Puzzle.part2(input)}")

object Puzzle:
  def part1(input: Vector[String]): Int = input.map(IP.apply).count(_.supportsTLS)
  def part2(input: Vector[String]): Int = input.map(IP.apply).count(_.supportsSSL)

extension(s: String)
  def containsABBA: Boolean =
    if s.length <= 3 then false
    else s.sliding(4).toVector.exists(v => v(0) == v(3) && v(1) == v(2) && v(0) != v(1))

  def findAllABAPatterns: Vector[(String, String)] =
    if s.length <= 2 then Vector.empty[(String, String)]
    else s.sliding(3).toVector
      .filter(v => v(0) == v(2) && v(0) != v(1)).map(_.take(2).splitAt(1))

  def padBrackets: String =
    s.replace("[", " [").replace("]","] ")

case class IP(address: String):
  val (hypernetSequences, supernetSequences) = address
    .padBrackets
    .split(" ")
    .toVector
    .partition(_.startsWith("["))

  def supportsTLS: Boolean = supernetSequences.exists(_.containsABBA) &&
    !hypernetSequences.exists(_.containsABBA)

  def supportsSSL: Boolean = supernetSequences
    .map(findAllABAPatterns)
    .filterNot(_.isEmpty)
    .flatMap(v => v.map((a, b) => s"$b$a$b"))
    .exists(bab => hypernetSequences.exists(_.contains(bab)))
