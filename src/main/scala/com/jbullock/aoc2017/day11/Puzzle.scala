package com.jbullock.aoc2017.day11

import scala.io.Source

@main
def solvePuzzle(): Unit =
  println(s"Part 1: ${Puzzle.part1}")
  println(s"Part 2: ${Puzzle.part2}")

object Puzzle:
  val input: Vector[String] = Source.fromResource("aoc/2017/Day11/Murray.txt").mkString.stripTrailing.split(",")
    .toVector
  val output: State = input.foldLeft(State(0,0,0,0))((s, d) => s.next(d))
  def part1: Int = output.distance
  def part2: Int = output.maxDistance

case class State(norths: Int, southWests: Int, southEasts: Int, maxDistance: Int):
  def distance: Int = (southWests - norths).abs + (southEasts - norths).abs
  def next(direction: String): State =
    val next = direction match
      case "n" => this.copy(norths = norths + 1)
      case "s" => this.copy(norths = norths - 1)
      case "sw" => this.copy(southWests = southWests + 1)
      case "ne" => this.copy(southWests = southWests - 1)
      case "se" => this.copy(southEasts = southEasts + 1)
      case "nw" => this.copy(southEasts = southEasts - 1)
      case _ => throw new RuntimeException(s"Unexpected direction: $direction")
    next.copy(maxDistance = this.maxDistance.max(next.distance))
