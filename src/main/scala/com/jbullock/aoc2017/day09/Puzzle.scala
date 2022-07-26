package com.jbullock.aoc2017.day09

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  println(s"Part 1: ${Puzzle.part1}")
  println(s"Part 2: ${Puzzle.part2}")

object Puzzle:
  val input: String = Source.fromResource("aoc/2017/Day09/Input.txt").mkString
  val output: State = processStream(input)
  def part1: Int = output.score
  def part2: Int = output.removed

enum Mode:
  case Group, Garbage

case class State(score: Int, removed: Int)

def processStream(input: String): State =

  @tailrec
  def loop(s: String, mode: Mode, level: Int, state: State): State =
    (s.headOption, mode) match
      case (Some(c), Mode.Group) => c match
        case '{' => loop(s.drop(1), Mode.Group,   level + 1, state)
        case '}' => loop(s.drop(1), Mode.Group,   level - 1, State(state.score + level, state.removed))
        case '<' => loop(s.drop(1), Mode.Garbage, level,     state)
        case _   => loop(s.drop(1), Mode.Group,   level,     state)
      case (Some(c), Mode.Garbage) => c match
        case '!' => loop(s.drop(2), Mode.Garbage, level, state)
        case '>' => loop(s.drop(1), Mode.Group,   level, state)
        case _   => loop(s.drop(1), Mode.Garbage, level, State(state.score, state.removed + 1))
      case (None, _) => state

  loop(input, Mode.Group, 0, State(0, 0))

