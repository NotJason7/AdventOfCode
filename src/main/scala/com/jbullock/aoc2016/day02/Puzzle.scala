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

abstract class Numpad:
  def name: String
  def up: Numpad
  def down: Numpad
  def left: Numpad
  def right: Numpad

abstract class ArcaneNumpad:
  def name: String
  def up: ArcaneNumpad
  def down: ArcaneNumpad
  def left: ArcaneNumpad
  def right: ArcaneNumpad

case object One extends ArcaneNumpad:
  val name = "1"
  override def up: ArcaneNumpad = One
  override def down: ArcaneNumpad = Three
  override def left: ArcaneNumpad = One
  override def right: ArcaneNumpad = One
case object Two extends ArcaneNumpad:
  val name = "2"
  override def up: ArcaneNumpad = Two
  override def down: ArcaneNumpad = Six
  override def left: ArcaneNumpad = Two
  override def right: ArcaneNumpad = Three
case object Three extends ArcaneNumpad:
  val name = "3"
  override def up: ArcaneNumpad = One
  override def down: ArcaneNumpad = Seven
  override def left: ArcaneNumpad = Two
  override def right: ArcaneNumpad = Four
case object Four extends ArcaneNumpad:
  val name = "4"
  override def up: ArcaneNumpad = Four
  override def down: ArcaneNumpad = Eight
  override def left: ArcaneNumpad = Three
  override def right: ArcaneNumpad = Four
case object Five extends ArcaneNumpad:
  val name = "5"
  override def up: ArcaneNumpad = Five
  override def down: ArcaneNumpad = Five
  override def left: ArcaneNumpad = Five
  override def right: ArcaneNumpad = Six
case object Six extends ArcaneNumpad:
  val name = "6"
  override def up: ArcaneNumpad = Two
  override def down: ArcaneNumpad = A
  override def left: ArcaneNumpad = Five
  override def right: ArcaneNumpad = Seven
case object Seven extends ArcaneNumpad:
  val name = "7"
  override def up: ArcaneNumpad = Three
  override def down: ArcaneNumpad = B
  override def left: ArcaneNumpad = Six
  override def right: ArcaneNumpad = Eight
case object Eight extends ArcaneNumpad:
  val name = "8"
  override def up: ArcaneNumpad = Four
  override def down: ArcaneNumpad = C
  override def left: ArcaneNumpad = Seven
  override def right: ArcaneNumpad = Nine
case object Nine extends ArcaneNumpad:
  val name = "9"
  override def up: ArcaneNumpad = Nine
  override def down: ArcaneNumpad = Nine
  override def left: ArcaneNumpad = Eight
  override def right: ArcaneNumpad = Nine
case object A extends ArcaneNumpad:
  val name = "A"
  override def up: ArcaneNumpad = Six
  override def down: ArcaneNumpad = A
  override def left: ArcaneNumpad = A
  override def right: ArcaneNumpad = B
case object B extends ArcaneNumpad:
  val name = "B"
  override def up: ArcaneNumpad = Seven
  override def down: ArcaneNumpad = D
  override def left: ArcaneNumpad = A
  override def right: ArcaneNumpad = C
case object C extends ArcaneNumpad:
  val name = "C"
  override def up: ArcaneNumpad = Eight
  override def down: ArcaneNumpad = C
  override def left: ArcaneNumpad = B
  override def right: ArcaneNumpad = C
case object D extends ArcaneNumpad:
  val name = "D"
  override def up: ArcaneNumpad = B
  override def down: ArcaneNumpad = D
  override def left: ArcaneNumpad = D
  override def right: ArcaneNumpad = D



case object OnePad extends Numpad:
  val name = "1"
  override def up: Numpad = OnePad
  override def down: Numpad = FourPad
  override def left: Numpad = OnePad
  override def right: Numpad = TwoPad
case object TwoPad extends Numpad:
  val name = "2"
  override def up: Numpad = TwoPad
  override def down: Numpad = FivePad
  override def left: Numpad = OnePad
  override def right: Numpad = ThreePad
case object ThreePad extends Numpad:
  val name = "3"
  override def up: Numpad = ThreePad
  override def down: Numpad = SixPad
  override def left: Numpad = TwoPad
  override def right: Numpad = ThreePad
case object FourPad extends Numpad:
  val name = "4"
  override def up: Numpad = OnePad
  override def down: Numpad = SevenPad
  override def left: Numpad = FourPad
  override def right: Numpad = FivePad
case object FivePad extends Numpad:
  val name = "5"
  override def up: Numpad = TwoPad
  override def down: Numpad = EightPad
  override def left: Numpad = FourPad
  override def right: Numpad = SixPad
case object SixPad extends Numpad:
  val name = "6"
  override def up: Numpad = ThreePad
  override def down: Numpad = NinePad
  override def left: Numpad = FivePad
  override def right: Numpad = SixPad
case object SevenPad extends Numpad:
  val name = "7"
  override def up: Numpad = FourPad
  override def down: Numpad = SevenPad
  override def left: Numpad = SevenPad
  override def right: Numpad = EightPad
case object EightPad extends Numpad:
  val name = "8"
  override def up: Numpad = FivePad
  override def down: Numpad = EightPad
  override def left: Numpad = SevenPad
  override def right: Numpad = NinePad
case object NinePad extends Numpad:
  val name = "9"
  override def up: Numpad = SixPad
  override def down: Numpad = NinePad
  override def left: Numpad = EightPad
  override def right: Numpad = NinePad

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction

def followDirectionNumpad(directions: List[Direction], start: Numpad): Numpad =
  directions.foldLeft(start: Numpad){
    (n: Numpad, d: Direction) => d match
      case Up => n.up
      case Down => n.down
      case Left => n.left
      case Right => n.right
  }

def followDirectionArcane(directions: List[Direction], start: ArcaneNumpad): ArcaneNumpad =
  directions.foldLeft(start: ArcaneNumpad){
    (a: ArcaneNumpad, d: Direction) => d match
      case Up => a.up
      case Down => a.down
      case Left => a.left
      case Right => a.right
  }

def followDirectionsNumpad(directions: List[List[Direction]], start: Numpad): List[Numpad] =

  @tailrec
  def loop(directions: List[List[Direction]], digits: List[Numpad]): List[Numpad] =
    if directions.isEmpty then digits.tail
    else
      val nextDirections = directions.head
      val nextStart = digits.last
      val nextNumpad = followDirectionNumpad(nextDirections, nextStart)
      loop(directions.tail, digits :+ nextNumpad)

  loop(directions, List(start))

def followDirectionsArcane(directions: List[List[Direction]], start: ArcaneNumpad): List[ArcaneNumpad] =

  @tailrec
  def loop(directions: List[List[Direction]], digits: List[ArcaneNumpad]): List[ArcaneNumpad] =
    if directions.isEmpty then digits.tail
    else
      val nextDirections = directions.head
      val nextStart = digits.last
      val nextArcane = followDirectionArcane(nextDirections, nextStart)
      loop(directions.tail, digits :+ nextArcane)

  loop(directions, List(start))

object Puzzle:

  def part1(input: List[String]): Int =
    val start = FivePad
    val directions = input.map(_.toList).map(_.map{ d =>
      d match
        case 'U' => Up
        case 'L' => Left
        case 'R' => Right
        case 'D' => Down
        case e => throw new Exception(s"Unexpected character $e!")
    })
    followDirectionsNumpad(directions, start).map(_.name).mkString.toInt

  def part2(input: List[String]): String =
    val start = Five
    val directions = input.map(_.toList).map(_.map{ d =>
      d match
        case 'U' => Up
        case 'L' => Left
        case 'R' => Right
        case 'D' => Down
        case e => throw new Exception(s"Unexpected character $e!")
    })
    followDirectionsArcane(directions, start).map(_.name).mkString
