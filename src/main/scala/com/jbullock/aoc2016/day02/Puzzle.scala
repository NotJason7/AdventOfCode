package com.jbullock.aoc2016.day02

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: List[String] = Source
    .fromResource("aoc/2016/Day02/Input.txt")
    .getLines
    .toList
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
  val part2Answer = Puzzle.part2(input)
  println(s"Part2: $part2Answer")

enum Direction:
  case Up, Down, Left, Right

case class Position(x: Int, y: Int)
extension(p: Position)
  def up: Position = Position(p.x, p.y-1)
  def down: Position = Position(p.x, p.y+1)
  def left: Position = Position(p.x-1, p.y)
  def right: Position = Position(p.x+1, p.y)

case class Numpad(grid: Map[Position, String])
object Numpad:
  def fromLists(lists: List[List[String]]): Numpad =
    val grid = for {
      y <- lists.indices.toList
      x <- lists.head.indices.toList
    } yield Map(Position(x, y) -> lists(y)(x))
    Numpad(grid.flatten.toMap)
extension(n: Numpad)
  def buttonMove(p: Position, direction: Direction): Position =
    val nextPosition = direction match
      case Direction.Up => p.up
      case Direction.Down => p.down
      case Direction.Left => p.left
      case Direction.Right => p.right
    if n.grid.keys.toList.contains(nextPosition) && n.grid(nextPosition).nonEmpty then nextPosition else p

def followDirection(directions: List[Direction], n: Numpad, start: Position): Position =
  directions.foldLeft(start){(p: Position, d: Direction) => n.buttonMove(p, d)}

def followDirections(directions: List[List[Direction]], numpad: Numpad, start: Position): String =
  @tailrec
  def loop(directions: List[List[Direction]], positions: List[Position]): List[Position] =
    if directions.isEmpty then positions.tail
    else
      val nextDirections = directions.head
      val nextNumpad = followDirection(nextDirections, numpad, positions.last)
      loop(directions.tail, positions :+ nextNumpad)

  val positions = loop(directions, List(start))
  positions.map(numpad.grid(_)).mkString

def readDirections(input: List[String]): List[List[Direction]] =
  input.map(_.toList).map(_.map{ _ match
      case 'U' => Direction.Up
      case 'D' => Direction.Down
      case 'L' => Direction.Left
      case 'R' => Direction.Right
      case e => throw new Exception(s"Unexpected character $e!")
  })

object Puzzle:

  def part1(input: List[String]): String =
    val numpad = Numpad.fromLists(List(
        List("1", "2", "3"),
        List("4", "5", "6"),
        List("7", "8", "9")))
    val start = Position(1,1)
    val directions = readDirections(input)
    followDirections(directions, numpad, start).mkString

  def part2(input: List[String]): String =
    val numpad = Numpad.fromLists(List(
        List("",   "", "1",  "",  ""),
        List("",  "2", "3", "4",  ""),
        List("5", "6", "7", "8", "9"),
        List("",  "A", "B", "C",  ""),
        List("",   "", "D",  "",  "")))
    val start = Position(0,2)
    val directions = readDirections(input)
    followDirections(directions, numpad, start).mkString
