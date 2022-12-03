package com.jbullock.aoc2022.day02

@main def solvePuzzle(): Unit =
  val input = io.Source.fromResource("aoc/2022/Day02/Input.txt").getLines.toVector
  val games = input.map(Game.fromMovesString)
  val part1 = games.map(_.score).sum
  println(s"Part 1: $part1")
  val relativeGames = input.map(Game.fromRelativeMoveString)
  val part2 = relativeGames.map(_.score).sum
  println(s"Part 2: $part2")

sealed trait Move(val beats: Move, val losesTo: Move, val points: Int)
case object Rock extends Move(Scissors, Paper,1)
case object Paper extends Move(Rock, Scissors,2)
case object Scissors extends Move(Paper, Rock, 3)

extension(s: String)
  def toMove: Move = s match
    case "A" | "X" => Rock
    case "B" | "Y" => Paper
    case "C" | "Z" => Scissors
  def toRelativeMove(m: Move): Move = s match
    case "X" => m.beats
    case "Y" => m
    case "Z" => m.losesTo

case class Game(opponent: Move, you: Move):
  def score: Int =
    val (win, draw) = (6, 3)
    if you.beats == opponent then win + you.points
    else if you == opponent then draw + you.points
    else you.points
object Game:
  def fromMovesString(s: String): Game =
    val Vector(opponent, you) = s.split(" ").toVector.map(_.toMove)
    Game(opponent, you)
  def fromRelativeMoveString(s: String): Game =
    val Vector(opponentString, youString) = s.split(" ").toVector
    val opponent = opponentString.toMove
    val you = youString.toRelativeMove(opponent)
    Game(opponent, you)



