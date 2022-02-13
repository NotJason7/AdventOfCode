package com.jbullock.aoc2021.day13

import com.jbullock.aoc2021.day12.Puzzle

import breeze.plot._

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: List[String] = Source
    .fromResource("2021/Day13/Input.txt")
    .getLines
    .toList
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
  val part2Answer: Unit = Puzzle.part2(input)

case class Dot(x: Int, y: Int)
extension(d: Dot){
  def up(y: Int): Option[Dot] =
    if d.y == y then None
    else
      val foldDistance = d.y - y
      if foldDistance > 0 then
        Some(Dot(d.x,  d.y - (2 * foldDistance)))
      else Some(d)
  def left(x: Int): Option[Dot] =
    if d.x == x then None
    else
      val foldDistance = d.x - x
      if foldDistance > 0 then
        Some(Dot(d.x - (2 * foldDistance),  d.y))
      else Some(d)
}

case class Paper(dots: Set[Dot])
extension(p: Paper){
  def up(y: Int): Paper =
    Paper(p.dots.flatMap(_.up(y)))
  def left(x: Int): Paper =
    Paper(p.dots.flatMap(_.left(x)))
}

sealed trait Instruction
case class Up(y: Int) extends Instruction
case class Left(x: Int) extends Instruction

object Puzzle:

  def part1(input: List[String]): Int =
    val coordinates = input.filterNot(_.startsWith("fold")).filterNot(_.isEmpty)
    val instructions = parseInstructions(input.filter(_.startsWith("fold")))
    val paper = parseCoordinates(coordinates)
    val folded = foldPaper(paper, instructions.head)
    folded.dots.size

  def part2(input: List[String]): Unit =
    val coordinates = input.filterNot(_.startsWith("fold")).filterNot(_.isEmpty)
    val instructions = parseInstructions(input.filter(_.startsWith("fold")))
    val paper = parseCoordinates(coordinates)
    val folded = loop(paper, instructions)
    val finalDots = folded.dots.toList
    val xs = finalDots.map(d => d.x)
    val ys = finalDots.map(d => -d.y)

    val f = Figure()
    val p = f.subplot(0)
    p += plot(xs, ys, '+')
    p.title = "AOC2021 - Day 13"
    f.saveas("src/main/scala/com/jbullock/aoc2021/day13/Part2.png")
    ()

  def foldPaper(paper: Paper, instruction: Instruction): Paper = instruction match
    case Up(y) => paper.up(y)
    case Left(x) => paper.left(x)

  @tailrec
  def loop(paper: Paper, instructions: List[Instruction]): Paper =
    if instructions.isEmpty then paper
    else
      loop(foldPaper(paper, instructions.head), instructions.tail)


  def parseCoordinates(input: List[String]): Paper =
    Paper(input
      .map(_.split(',')
        .toList
        .map(_.toInt))
      .map{ case List(x, y) => Dot(x, y) }
      .toSet
    )

  def parseInstructions(input: List[String]): List[Instruction] =
    input.map {
      _.replace("fold along ", "")
        .split('=').toList
    }
    .map(z => (z.head, z.tail.head.toInt))
    .map{ case (direction, line) =>
      direction match
        case "y" => Up(line)
        case "x" => Left(line)
    }


