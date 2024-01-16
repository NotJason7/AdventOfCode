package com.jbullock.aoc2023.day02

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2023/Day02/Input.txt").getLines.toVector
  val games = input.flatMap(Game.fromGameString)
  val part1 = games.filter(_.isPossibleGame(12, 13, 14)).map(_.id).sum
  println(s"Part 1: $part1")
  val part2 = games.map(_.power).sum
  println(s"Part 2: $part2")

enum Colour:
  case Red, Blue, Green

case class Game(id: Int, rounds: Vector[Map[Colour, Int]]):
  def maxOfColourSeen(colour: Colour): Int = rounds.map(_.getOrElse(colour, 0)).max
  def isPossibleGame(reds: Int, greens: Int, blues: Int): Boolean =
    maxOfColourSeen(Colour.Red) <= reds
      && maxOfColourSeen(Colour.Blue) <= blues
      && maxOfColourSeen(Colour.Green) <= greens
  def fewestCubes: Map[Colour, Int] = Map(
    Colour.Red   -> maxOfColourSeen(Colour.Red),
    Colour.Green -> maxOfColourSeen(Colour.Green),
    Colour.Blue  -> maxOfColourSeen(Colour.Blue)
  )
  def power: Int = fewestCubes.values.product
object Game:
  def fromGameString(gameString: String): Option[Game] = gameString match
    case s"Game $gameIndex: $gameString" =>
      val roundStrings = gameString.split("; ").toVector
      val cubeNumbers = roundStrings.map(_.split(", ").toVector.map {
        case s"$quantity red"   => (Colour.Red, quantity.toInt)
        case s"$quantity blue"  => (Colour.Blue, quantity.toInt)
        case s"$quantity green" => (Colour.Green, quantity.toInt)
      }.toMap)
      Some(Game(gameIndex.toInt, cubeNumbers))
    case _ => None
