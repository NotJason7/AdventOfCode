package com.jbullock.aoc2021.day13

import com.jbullock.aoc2021.day12.Puzzle

import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: List[String] = Source
    .fromResource("2021/Day13/Example.txt")
    .getLines
    .toList
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
//  val part2Answer = Puzzle.part2(input)
//  println(s"Part 2: $part2Answer")

case class Dot(x: Int, y: Int)
extension(d: Dot){
  def upFold(y: Int, height: Int): Option[Dot] =
    if d.y == y then None
    else if d.y > y then Some(Dot(d.x, height - d.y))
    else Some(d)
  def leftFold(x: Int, width: Int): Option[Dot] =
    if d.x == x then None
    else if d.x > x then Some(Dot(width - d.x, d.y))
    else Some(d)
}

case class Paper(height: Int, width: Int, dots: Set[Dot])

object Puzzle:

  def part1(input: List[String]): Int =
    println(input)
    1

  def part2(input: List[String]): Int = ???

  def generatePaper(input: List[String]) = ???
