package com.jbullock.aoc2021.day15

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class PuzzleSpec extends AnyFlatSpec with Matchers {
  val example: List[String] = Source
    .fromResource("2021/Day15/Example.txt")
    .getLines
    .toList

  "Part1" must "give correct answer for example" in {
    val answer = Puzzle.part1(example)
    val expected = 40
    answer mustBe expected
  }

}
