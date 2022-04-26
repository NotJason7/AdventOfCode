package com.jbullock.aoc2016.day08

import scala.io.Source
import doodle.image._
import doodle.image.syntax.all._
import doodle.core._
import doodle.java2d._
import doodle.effect.Writer._
import cats.effect.unsafe.implicits.global

@main
def solvePuzzle(): Unit =
  val input: Vector[String] = Source.fromResource("aoc/2016/Day08/Input.txt").getLines.toVector
  println(s"Part1: ${Puzzle.part1(input)}")
  println("Part2:")
  Puzzle.part2(input)

object Puzzle:
  def part1(input: Vector[String]): Int =
    val start = Screen(50, 6, Set.empty[Pixel])
    start.parseInstructions(input).pixels.size

  def part2(input: Vector[String]): Unit =
    val start = Screen(50, 6, Set.empty[Pixel])
    val end = start.parseInstructions(input)
    end.post()

case class Pixel(x: Int, y: Int)

case class Screen(width: Int, height: Int, pixels: Set[Pixel]):
  def post(): Unit =
    val chars = for {
      y <- 0 until height
      x <- 0 until width
    } yield if pixels.contains(Pixel(x, y)) then "██" else "  "
    chars.mkString.grouped(100).toVector.foreach(println)

//  def draw(name: String): Unit =
//    val point = Image.rectangle(10,10).fillColor(Color.black)
//    val screen = pixels.map(p => point.at(10*p.x, -10*p.y)).reduce(_.on(_))
//    screen.write[Png](s"src/main/scala/com/jbullock/aoc2016/day08/$name.png")

  def rectangle(rectangleWidth: Int, rectangleHeight: Int): Screen =
    val rectanglePixels = for {
      y <- 0 until rectangleHeight
      x <- 0 until rectangleWidth
    } yield Pixel(x, y)
    Screen(width, height, pixels ++ rectanglePixels)

  def rotateColumn(column: Int, magnitude: Int): Screen =
    Screen(width, height, pixels.map{ p =>
      if p.x == column then Pixel(p.x, (p.y + magnitude) % height) else p
    })

  def rotateRow(row: Int, magnitude: Int): Screen =
    Screen(width, height, pixels.map{ p =>
      if p.y == row then Pixel((p.x + magnitude) % width, p.y) else p
    })

  def parseInstruction(instruction: String): Screen =
    if instruction.startsWith("rect ") then
      val rectangleInstruction = instruction.drop(5).split('x').map(_.toInt)
      rectangle(rectangleInstruction(0), rectangleInstruction(1))
    else if instruction.startsWith("rotate column x=") then
      val columnInstruction = instruction.drop(16).split(" by ").map(_.toInt)
      rotateColumn(columnInstruction(0), columnInstruction(1))
    else if instruction.startsWith("rotate row y=") then
      val rowInstruction = instruction.drop(13).split(" by ").map(_.toInt)
      rotateRow(rowInstruction(0), rowInstruction(1))
    else throw new RuntimeException(s"Encountered an unparsable instruction: $instruction")

  def parseInstructions(instructions: Vector[String]): Screen =
    instructions.foldLeft(this)((screen, instruction) => screen.parseInstruction(instruction))

