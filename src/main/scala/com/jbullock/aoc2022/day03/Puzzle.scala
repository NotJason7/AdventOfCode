package com.jbullock.aoc2022.day03

@main def solvePuzzle(): Unit =
  val input = io.Source.fromResource("aoc/2022/Day03/Input.txt").getLines.toVector
  val rucksacks = input.map(Rucksack.apply)
  val priorities = (('a' to 'z') ++ ('A' to 'Z')).zipWithIndex.map(x => (x._1, x._2 + 1)).toMap
  val errors = rucksacks.flatMap(rucksack => priorities.get(rucksack.error))
  println(s"Part 1: ${errors.sum}")
  val groups = rucksacks.grouped(3).map(_.map(_.contents.toSet).reduce(_.intersect(_)).head).flatMap(priorities.get).sum
  println(s"Part 2: $groups")

case class Rucksack(contents: String):
  val (compartmentA, compartmentB) = contents.splitAt(contents.length/2)
  def error: Char = compartmentA.toSet.intersect(compartmentB.toSet).head
