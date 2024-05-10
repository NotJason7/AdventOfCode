package com.jbullock.aoc2021_2.day02

@main def solvePuzzle(): Unit =
  val commands = scala.io.Source.fromResource("aoc/2021/Day02/Input.txt").getLines.toSeq.map(Command.fromString)
  val part1    = commands.reduce(_ combine _).score
  val part2    = commands.foldLeft(Trajectory(0, 0, 0))((current, next) => current.runCommand(next)).score
  println(s"Part 1: $part1")
  println(s"Part 2: $part2")

case class Command(x: Int, y: Int):
  def combine(d: Command): Command = Command(x + d.x, y + d.y)
  def score: Int                   = x * y
object Command:
  def fromString(s: String): Command = s match
    case s"forward $x" => Command(x.toInt, 0)
    case s"down $y"    => Command(0, y.toInt)
    case s"up $y"      => Command(0, -y.toInt)
    case _             => throw IllegalArgumentException(s"Unable to create command from ${s}")

case class Trajectory(aim: Int, x: Int, y: Int):
  def runCommand(d: Command): Trajectory =
    val nextAim = aim + d.y
    val nextX   = x + d.x
    val nextY   = y + (d.x * nextAim)
    Trajectory(nextAim, nextX, nextY)
  def score: Int = x * y
