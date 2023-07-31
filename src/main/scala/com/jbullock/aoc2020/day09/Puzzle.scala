package com.jbullock.aoc2020.day09

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input         = scala.io.Source.fromResource("aoc/2020/Day09/Input.txt").getLines.toVector.map(_.toLong)
  val preamble      = 25
  val windows       = input.sliding(preamble + 1).toVector.map(Window.fromVector)
  val invalidNumber = windows.find(window => !window.isValid).map(_.value).get
  println(s"Part 1: $invalidNumber")
  val weakness = findWeakness(input, invalidNumber).get
  println(s"Part 2: $weakness")

case class Window(preamble: Vector[Long], value: Long):
  def isValid: Boolean = preamble.combinations(2).exists(_.sum == value)
object Window:
  def fromVector(v: Vector[Long]): Window = Window(v.take(v.length - 1), v.last)

def findWeakness(v: Vector[Long], target: Long): Option[Long] =
  @tailrec def loop(length: Int): Option[Long] =
    if length > v.length then None
    else
      v.sliding(length).find(_.sum == target) match
        case Some(contiguousSet) => Some(contiguousSet.min + contiguousSet.max)
        case None                => loop(length + 1)
  loop(2)
