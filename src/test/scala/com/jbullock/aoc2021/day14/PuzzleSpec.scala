package com.jbullock.aoc2021.day14

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class PuzzleSpec extends AnyFlatSpec with Matchers {
  val example: List[String] = Source
    .fromResource("2021/Day14/Example.txt")
    .getLines
    .toList

  "Part1" must "give correct answer for example" in {
    val answer = Puzzle.part1(example)
    val expected = 1588
    answer mustBe expected
  }
  
  "Part2" must "give correct answer for example" in {
    val answer = Puzzle.part2(example)
    val expected = 2188189693529L
    answer mustBe expected
  }

  "Polymer" must "apply rules correctly" in {
    val start = Polymer(example.head)
    val rules = PairInsertion(example.drop(2).map{ raw =>
      val keyValue = raw.split(" -> ").toList
      keyValue.head -> keyValue.tail.head
    }.foldLeft(Map[String,String]())(_ + _))
    val apply1 = start.applyRules(rules)
    val apply2 = apply1.applyRules(rules)
    val apply3 = apply2.applyRules(rules)
    val apply4 = apply3.applyRules(rules)
    val apply2MultiLoop = start.loopApply(rules, 2)
    val apply3MultiLoop = start.loopApply(rules, 3)
    val apply4MultiLoop = start.loopApply(rules, 4)
    val expected1 = Polymer("NCNBCHB")
    val expected2 = Polymer("NBCCNBBBCBHCB")
    val expected3 = Polymer("NBBBCNCCNBBNBNBBCHBHHBCHB")
    val expected4 = Polymer("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")

    apply1 mustBe expected1
    apply2 mustBe expected2
    apply3 mustBe expected3
    apply4 mustBe expected4
    apply2MultiLoop mustBe expected2
    apply3MultiLoop mustBe expected3
    apply4MultiLoop mustBe expected4
  }
}
