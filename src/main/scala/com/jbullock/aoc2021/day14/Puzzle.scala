package com.jbullock.aoc2021.day14

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

@main
def solvePuzzle(): Unit =
  val input: List[String] = Source
    .fromResource("2021/Day14/Input.txt")
    .getLines
    .toList
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
  val part2Answer = Puzzle.part2(input)
  println(s"Part2: $part2Answer")

case class PolymerLong(first: String, last: String, elements: Map[String, Long])
object PolymerLong:
  def fromString(s: String): PolymerLong =
    val first = s.head.toString
    val last = s.last.toString
    val elements = s.sliding(2).toList.groupBy(identity).view.mapValues(_.size.toLong).toMap
    PolymerLong(first, last, elements)

extension(p: PolymerLong)
  def applyRules(pi: PairInsertion): PolymerLong =
    val elements = p.elements.toList.flatMap{
      case (chain, count) =>
        val insert = pi.rules(chain)
        val startInsert = (chain.head + insert, count)
        val insertEnd = (insert + chain.last, count)
        List(startInsert, insertEnd)
      }
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).sum).toMap
    PolymerLong(p.first, p.last, elements)

  def loopApply(pi: PairInsertion, n: Int): PolymerLong =
    @tailrec
    def loop(p: PolymerLong, pi: PairInsertion, n: Int): PolymerLong =
      if n <= 0 then p
      else loop(p.applyRules(pi), pi, n-1)
    loop(p, pi, n)

  def elementCount: List[(String,Long)] =
    val firstLast = List((p.first, 1L), (p.last, 1L))
    val elementList = p.elements.toList.flatMap((k, v) => List((k(0).toString, v), (k(1).toString, v)))
    val fullList = firstLast ++ elementList
    val doubleCount = fullList.foldLeft(Map[String, Long]()){ (count, entry) =>
      val key = entry._1
      val value = entry._2 + count.getOrElse(key, 0L)
      count + (key -> value)
    }
    doubleCount.view.mapValues(_ / 2).toList

  def mostFrequent: (String, Long) =
    p.elementCount.maxBy(_._2)

  def leastFrequent: (String, Long) =
    p.elementCount.minBy(_._2)





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
object PairInsertion:
  def fromList(ls: List[String]): PairInsertion =
    val rules = ls.map{ raw =>
      val keyValue = raw.split(" -> ").toList
      keyValue.head -> keyValue.tail.head
    }.foldLeft(Map[String,String]())(_ + _)
    PairInsertion(rules)



object Puzzle:

  def part1(input: List[String]): Long =
    val start = Polymer(input.head)
    val rules = PairInsertion.fromList(input.drop(2))
    val end = start.loopApply(rules, 10)
    val frequency = end.elements.toList.groupBy(identity).map((k, v) => v.size.toLong).toList.sorted
    frequency.last - frequency.head

  def part2(input: List[String]): Long =
    val start = PolymerLong.fromString(input.head)
    val rules = PairInsertion.fromList(input.drop(2))
    val end = start.loopApply(rules, 40)
    end.mostFrequent._2 - end.leastFrequent._2
