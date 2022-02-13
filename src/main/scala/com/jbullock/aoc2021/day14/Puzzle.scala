package com.jbullock.aoc2021.day14

import com.jbullock.aoc2021.day12.Puzzle

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: List[String] = Source
    .fromResource("2021/Day14/Example.txt")
    .getLines
    .toList
//  val part1Answer = Puzzle.part1(input)
//  println(s"Part1: $part1Answer")
  val part2Answer: Unit = Puzzle.part2(input)
  println(s"Part2: $part2Answer")

case class PolymerLong(first: Char, last: Char, elements: Map[(Char, Char),Long])
extension(p: PolymerLong)
  def applyRules(pi: PairInsertionLong): PolymerLong =
    val elements = p.elements.toList.flatMap{
      case ((start, end), count) =>
        val insert = pi.rules((start, end))(0)
        List(((start, insert), count), ((insert, end), count))
    }.toMap
    PolymerLong(p.first, p.last, elements)

  def loopApply(pi: PairInsertionLong, n: Int): PolymerLong =
    @tailrec
    def loop(p: PolymerLong, pi: PairInsertionLong, n: Int): PolymerLong =
      if n <= 0 then p
      else loop(p.applyRules(pi), pi, n-1)
    loop(p, pi, n)

  def elementCount(): List[(Char,Long)] =
    p.elements
      .flatMap((k, v) => List(Map(k._1 -> v), Map(k._2 -> v)))
      .foldLeft(Map[Char, Long](p.first -> 1, p.last -> 1)){ (count: Map[Char, Long], update) =>
        val key = update.keys.head
        val value = update.values.head + count.getOrElse(key, 0L)
        count + (key -> value)
      }.map((k, v) => (k, v/2)).toList

case class Polymer(elements: String)
extension(p: Polymer)
  def applyRules(pi: PairInsertion): Polymer =
    val start = p.elements(0).toString
    Polymer(p.elements
      .sliding(2)
      .toList
      .map(s =>s"${pi.rules(s)}${s(1).toString}")
      .foldLeft(start)(_ + _))
  def loopApply(pi: PairInsertion, n: Int): Polymer =

    @tailrec
    def loop(p: Polymer, pi: PairInsertion, n: Int): Polymer =
      if n <= 0 then p
      else loop(p.applyRules(pi), pi, n-1)

    loop(p, pi, n)


case class PairInsertion(rules: Map[String, String])
case class PairInsertionLong(rules: Map[(Char, Char), String])


object Puzzle:

  def part1(input: List[String]): Long =
    val start = Polymer(input.head)
    val rules = PairInsertion(input.drop(2).map{ raw =>
      val keyValue = raw.split(" -> ").toList
      keyValue.head -> keyValue.tail.head
    }.foldLeft(Map[String,String]())(_ + _))
    val end = start.loopApply(rules, 10)
    val frequency = end.elements.toList.groupBy(identity).map((k, v) => v.size.toLong).toList.sorted
    frequency.last - frequency.head

  def part2(input: List[String]): Long =
    val first = input.head.head
    val last = input.head.last
    val elements = input.head.sliding(2).toList.map(l => (l.head, l.last) -> 1L).foldLeft(Map[(Char, Char), Long]())(_ + _)
    val start = PolymerLong(first, last, elements)
    println(start)
    val startCount = start.elementCount()
    println(start.elementCount())
    val rules = PairInsertionLong(input.drop(2).map{ raw =>
      val keyValue = raw.split(" -> ").toList
      val key = (keyValue.head.head, keyValue.head.last)
      val value = keyValue.tail.head
      key -> value
    }.foldLeft(Map[(Char, Char),String]())(_ + _))
    val end = start.loopApply(rules, 2)
    val endCount = end.elementCount()
    println(endCount)
    val frequency = end.elementCount().map(_._2).sorted
    println(frequency)
    val most = frequency.last
    val least = frequency.head
    println(most)
    println(least)
    most - least

