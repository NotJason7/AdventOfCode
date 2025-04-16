package com.jbullock.aoc2024.day05

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2024/Day05/Sample.txt").getLines.toSeq
  val rules = input
    .takeWhile(_ != "")
    .map { case s"$x|$y" =>
      (x.toInt, y.toInt)
    }
    .groupMap((x, y) => x)((x, y) => y)
    .map((x, ys) => Rule(x, ys))
    .toSeq
  val updates      = input.dropWhile(_ != "").drop(1).map(_.split(',').toSeq.map(_.toInt)).map(Update.apply)
  val validUpdates = updates.filter(update => rules.forall(rule => update.test(rule)))
  val part1        = validUpdates.map(_.middlePage).sum
  println(s"Part 1: $part1")
  val invalidUpdates      = updates.filter(update => rules.exists(rule => !update.test(rule)))
  val fixedInvalidUpdates = invalidUpdates.map(update => update.adhere(rules))
  val part2               = fixedInvalidUpdates.map(_.middlePage).sum
  println(s"Part 2: $part2")

case class Rule(key: Int, values: Seq[Int])

case class Update(pages: Seq[Int]):
  val middlePage: Int = pages(Math.ceil(pages.length / 2).toInt)
  def test(rule: Rule): Boolean =
    if pages.contains(rule.key) then
      val keyIndex = pages.indexOf(rule.key)
      rule.values.filter(pages.contains).forall(v => pages.indexOf(v) > keyIndex)
    else true
  def adhere(rules: Seq[Rule]): Update =
    val potentialUpdates = pages.permutations.map(Update.apply)
    rules.foldLeft(potentialUpdates)((updates, rule) => updates.filter(update => update.test(rule))).toSeq.head
