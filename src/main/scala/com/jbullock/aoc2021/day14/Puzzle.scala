//package com.jbullock.aoc2021.day14
//
//import com.jbullock.aoc2021.day12.Puzzle
//
//import scala.annotation.tailrec
//import scala.io.Source
//import scala.language.postfixOps
//
//@main
//def solvePuzzle(): Unit =
//  val input: List[String] = Source
//    .fromResource("2021/Day14/Input.txt")
//    .getLines
//    .toList
//  val part1Answer = Puzzle.part1(input)
//  println(s"Part1: $part1Answer")
//  val part2Answer = Puzzle.part2(input)
//  println(s"Part2: $part2Answer")
//
//case class PolymerLong(first: String, last: String, elements: Map[String, Long])
//extension(p: PolymerLong)
//  def applyRules(pi: PairInsertion): PolymerLong =
//    val elements = p.elements.toList.flatMap{
//      case (chain, count) =>
//        val insert = pi.rules(chain)
//        val startInsert = (chain.head + insert, count)
//        val insertEnd = (insert + chain.last, count)
//        List(startInsert, insertEnd)
//      }
//      .groupBy(_._1)
//      .view.mapValues(_.map(_._2).sum).toMap
//    PolymerLong(p.first, p.last, elements)
//
//  def loopApply(pi: PairInsertion, n: Int): PolymerLong =
//    @tailrec
//    def loop(p: PolymerLong, pi: PairInsertion, n: Int): PolymerLong =
//      if n <= 0 then p
//      else loop(p.applyRules(pi), pi, n-1)
//    loop(p, pi, n)
//
//  def elementCount(): List[(Char,Long)] =
//    val firstLast = List((p.first, 1L), (p.last, 1L))
//    val elementList = p.elements.toList.flatMap((k, v) => List((k(0), v), (k(1), v)))
//    val fullList = firstLast ++ elementList
//    val test = fullList.foldLeft(Map[String, Long]()){ (count, entry) =>
//      val key = entry._1
//      val value = entry._2 + count.getOrElse(entry._1, 0L)
//      count + (key -> value)
//    }
//    println(test)
////    val test = fullList.groupBy(_._1).view.mapValues(_.map(_._2)).toList
////    val test2 = test.map(x: (String, Long) => (x._1, x._2.sum))
////    val test = fullList
////      .flatMap((k, v) => List((k._1, v), (k._2, v)))
////      .groupBy(_._1).mapValues(_.map(_._2).sum).toList
////      .groupBy(_._1).map{ x=>
////      val counts = x._2.map(_._2)
////    }
////    println(test)
//    List(('l',1L))
////      .map{ m =>
////        val key = m._1
////        val instances = m._2.map(_._2)
////        val value = instances.sum
////        key -> value/2
////      }.toList
////      .foldLeft(Map[Char, Long](p.first -> 1L, p.last -> 1L)){ (count: Map[Char, Long], update) =>
////        val key = update.keys.head
////        val value: Long = update.values.head + count.getOrElse(key, 0L))
////        count + (key -> value)
////      }.map((k, v) => (k, v/2L)).toList
//
////  def mostFrequent(): (Char, Long) =
////    p.elementCount().maxBy(_._2)
////
////  def leastFrequent(): (Char, Long) =
////    p.elementCount().minBy(_._2)
//
//
//
//
//
//case class Polymer(elements: String)
//extension(p: Polymer)
//  def applyRules(pi: PairInsertion): Polymer =
//    val start = p.elements(0).toString
//    Polymer(p.elements
//      .sliding(2)
//      .toList
//      .map(s =>s"${pi.rules(s)}${s(1).toString}")
//      .foldLeft(start)(_ + _))
//  def loopApply(pi: PairInsertion, n: Int): Polymer =
//
//    @tailrec
//    def loop(p: Polymer, pi: PairInsertion, n: Int): Polymer =
//      if n <= 0 then p
//      else loop(p.applyRules(pi), pi, n-1)
//
//    loop(p, pi, n)
//
//
//case class PairInsertion(rules: Map[String, String])
//
//
//object Puzzle:
//
//  def part1(input: List[String]): Long =
//    val start = Polymer(input.head)
//    val rules = PairInsertion(input.drop(2).map{ raw =>
//      val keyValue = raw.split(" -> ").toList
//      keyValue.head -> keyValue.tail.head
//    }.foldLeft(Map[String,String]())(_ + _))
//    val end = start.loopApply(rules, 10)
//    val frequency = end.elements.toList.groupBy(identity).map((k, v) => v.size.toLong).toList.sorted
//    frequency.last - frequency.head
//
//  def part2(input: List[String]): Long =
//    val first = input.head.head.toString
//    val last = input.head.last.toString
//    val elements = input.head.sliding(2).toList.groupBy(identity).view.mapValues(_.size.toLong).toMap
//    val rules = PairInsertion(input.drop(2).flatMap{ raw =>
//      val keyValue = raw.split(" -> ").toList
//      List(keyValue.head -> keyValue.last)
//    }.toMap)
//    val start = PolymerLong(first, last, elements)
//    val next = start.applyRules(rules)
//    next.elementCount()
////    val end = start.loopApply(rules, 2)
////    val frequency = end.elementCount().map(_._2).sortWith(_ < _)
////    val most = frequency.last
////    val least = frequency.head
////    most - least
//    1L
