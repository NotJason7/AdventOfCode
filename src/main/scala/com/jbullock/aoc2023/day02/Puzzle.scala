package com.jbullock.aoc2023.day02

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2023/Day02/Input.txt").getLines.toVector
  val games = input.flatMap(Game.fromGameString)
  val part1 = games.filter(_.isPossibleGame(12, 13, 14)).map(_.id).sum
  println(s"Part 1: $part1")
  val part2 = games.map(_.power).sum
  println(s"Part 2: $part2")

enum Colour:
  case red, blue, green

case class Game(id: Int, rounds: Vector[Map[Colour, Int]]):
  def maxOfColourSeen(colour: Colour): Int = rounds.map(_.getOrElse(colour, 0)).max
  def isPossibleGame(reds: Int, greens: Int, blues: Int): Boolean =
    maxOfColourSeen(Colour.red) <= reds
      && maxOfColourSeen(Colour.blue) <= blues
      && maxOfColourSeen(Colour.green) <= greens
  def fewestCubes: Map[Colour, Int] = Colour.values.map(colour => colour -> maxOfColourSeen(colour)).toMap
  def power: Int                    = fewestCubes.values.product
object Game:
  def fromGameString(gameString: String): Option[Game] = gameString match
    case s"Game $gameIndex: $gameString" =>
      val roundStrings = gameString.split("; ").toVector
      val cubeNumbers = roundStrings.map(_.split(", ").toVector.map { case s"$quantity $colorString" =>
        (Colour.valueOf(colorString), quantity.toInt)
      }.toMap)
      Some(Game(gameIndex.toInt, cubeNumbers))
    case _ => None
