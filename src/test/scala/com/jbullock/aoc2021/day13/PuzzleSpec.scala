package com.jbullock.aoc2021.day13

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class PuzzleSpec extends AnyFlatSpec with Matchers {
  val example: List[String] = Source
    .fromResource("aoc/2021/Day13/Example.txt")
    .getLines
    .toList

  "Part1" must "give correct answer for example" in {
    val answer = Puzzle.part1(example)
    val expected = 17
    answer mustBe expected
  }

  "Dot" must "fold left correctly" in {
    val dotRange = (0 to 10).toList
    val foldLine = 5
    val hories = dotRange.map(Dot(_, 0))
    val actual = hories.flatMap(_.left(foldLine))
    val expectedDots = (0 to 4).toList.map(Dot(_, 0))
    val expected = expectedDots ::: expectedDots.reverse
    actual mustBe expected
  }
  it must "fold up correctly" in {
    val dotRange = (0 to 10).toList
    val foldLine = 5
    val verties = dotRange.map(Dot(0, _))
    val actual = verties.flatMap(_.up(foldLine))
    val expectedDots = (0 to 4).toList.map(Dot(0, _))
    val expected = expectedDots ::: expectedDots.reverse
    actual mustBe expected
  }

  "Paper" must "fold left correctly" in {
    val d22 = Dot(2, 2)
    val d28 = Dot(2, 8)
    val d82 = Dot(8, 2)
    val d88 = Dot(8, 8)
    val before = Paper(Set(d22, d28, d82, d88))
    val actual = before.left(5)
    val expected = Paper(Set(d22, d28))
    actual mustBe expected
  }
  it must "fold right correctly" in {
    val d22 = Dot(2, 2)
    val d28 = Dot(2, 8)
    val d82 = Dot(8, 2)
    val d88 = Dot(8, 8)
    val before = Paper(Set(d22, d28, d82, d88))
    val actual = before.up(5)
    val expected = Paper(Set(d22, d82))
    actual mustBe expected
  }

}
