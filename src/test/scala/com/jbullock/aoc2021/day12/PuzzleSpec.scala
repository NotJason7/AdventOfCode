package com.jbullock.aoc2021.day12

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class PuzzleSpec extends AnyFlatSpec with Matchers {
  val exampleInput1: List[String] = Source
    .fromResource("aoc/2021/Day12/Example01.txt")
    .getLines
    .toList
  val exampleInput2: List[String] = Source
    .fromResource("aoc/2021/Day12/Example02.txt")
    .getLines
    .toList
  val exampleInput3: List[String] = Source
    .fromResource("aoc/2021/Day12/Example03.txt")
    .getLines
    .toList

  "Part1" must "give correct answer for example 1" in {
    val answer = Puzzle.part1(exampleInput1)
    val expected = 10
    answer mustBe expected
  }
  it must "give correct answer for example 2" in {
    val answer = Puzzle.part1(exampleInput2)
    val expected = 19
    answer mustBe expected
  }
  it must "give correct answer for example 3" in {
    val answer = Puzzle.part1(exampleInput3)
    val expected = 226
    answer mustBe expected
  }

  "Part2" must "give correct answer for example 1" in {
    val answer = Puzzle.part2(exampleInput1)
    val expected = 36
    answer mustBe expected
  }
  it must "give correct answer for example 2" in {
    val answer = Puzzle.part2(exampleInput2)
    val expected = 103
    answer mustBe expected
  }
  it must "give correct answer for example 3" in {
    val answer = Puzzle.part2(exampleInput3)
    val expected = 3509
    answer mustBe expected
  }

  "Node" must "correctly assign a name" in {
    val name = "name"
    val node = Node(name)
    node.name mustBe name
  }
  it must "correctly identify big nodes" in {
    val small = Node("a").isBig
    val big = Node("B").isBig
    small mustBe false
    big mustBe true
  }
  it must "correctly identify the start node" in {
    val start = Node("start").isStart
    val other = Node("other").isStart
    start mustBe true
    other mustBe false
  }
  it must "correctly identify the end node" in {
    val actual = Node("end").isEnd
    val other = Node("other").isEnd
    actual mustBe true
    other mustBe false
  }

  "Route" must "correctly instantiate" in {
    val startList = List(Node("start"))
    val path = Route(startList).path
    path mustBe startList
  }
  it must "correctly extend" in {
    val start = Node("start")
    val startList = List(start)
    val startRoute = Route(startList)
    val extension = Node("a")
    val actual = startRoute.extend(extension).path
    val expected = extension :: startList
    actual mustBe expected
  }
  it must "correctly count duplicates" in {
    val noDuplicatesList = List(Node("a"), Node("b"), Node("c"))
    val noDuplicates = Route(noDuplicatesList).duplicateCount

    val oneDuplicateList = List(Node("a"), Node("a"), Node("b"), Node("c"))
    val oneDuplicate = Route(oneDuplicateList).duplicateCount

    val twoDuplicateList = List(Node("a"), Node("a"), Node("a"), Node("b"), Node("b"), Node("c"))
    val twoDuplicates = Route(twoDuplicateList).duplicateCount

    noDuplicates mustBe 0
    oneDuplicate mustBe 1
    twoDuplicates mustBe 2
  }
  it must "correctly count instances of node in list" in {
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val nodeList = List(a, a, a, b, b, c)
    val route = Route(nodeList)
    val aCount = route.nodeCount(a)
    val bCount = route.nodeCount(b)
    val cCount = route.nodeCount(c)
    val dCount = route.nodeCount(d)

    aCount mustBe 3
    bCount mustBe 2
    cCount mustBe 1
    dCount mustBe 0
  }
  it must "correctly identify which nodes are valid to extend the route" in {
    val start = Node("start")
    val a = Node("a")
    val b = Node("b")
    val nodeListNoDupes = List(start, a, b)
    val nodeListOneDupe = List(start, a, b, a)
    val nodeListTwoDupes = List(start, a, b, a, b)
    val noDupes = Route(nodeListNoDupes)
    val oneDupe = Route(nodeListOneDupe)
    val twoDupes = Route(nodeListTwoDupes)

    val aExtendNoDupes = noDupes.canExtendSmall(a)
    val bExtendNoDupes = noDupes.canExtendSmall(b)
    val aExtendOneDupe = oneDupe.canExtendSmall(a)
    val bExtendOneDupe = oneDupe.canExtendSmall(b)
    val aExtendTwoDupes = twoDupes.canExtendSmall(a)
    val bExtendTwoDupes = twoDupes.canExtendSmall(b)

    aExtendNoDupes mustBe true
    bExtendNoDupes mustBe true
    aExtendOneDupe mustBe false
    bExtendOneDupe mustBe false
    aExtendTwoDupes mustBe false
    bExtendTwoDupes mustBe false

  }


}
