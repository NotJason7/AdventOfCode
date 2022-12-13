package com.jbullock.aoc2022.day13

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input   = io.Source.fromResource("aoc/2022/Day13/Input.txt").getLines.toVector
  val signals = input.filterNot(_.isBlank).grouped(2).toVector.map(DistressSignal.fromVector2)
  val part1   = signals.map(_.inRightOrder).zipWithIndex.filter(_._1).map(_._2 + 1).sum
  println(s"Part 1: $part1")

def parseDistressString(s: String): Vector[Distress] =
  @tailrec def loop(remaining: String, accumulator: Accumulator): Vector[Distress] = remaining.headOption match
    case None      => accumulator.signals
    case Some('[') => loop(remaining.tail, accumulator.deeper)
    case Some(']') => loop(remaining.tail, accumulator.shallower)
    case Some(',') => loop(remaining.tail, accumulator)
    case Some(_) =>
      val intString     = remaining.takeWhile(c => !Set('[', ',', ']').contains(c))
      val nextRemaining = remaining.drop(intString.length)
      loop(nextRemaining, accumulator.addSignal(intString.toIntOption))

  loop(s, Accumulator(Vector.empty[Distress], 0, Vector(0)))

case class Distress(list: Int, level: Int, number: Option[Int]):
  override def toString: String = s"$list:${number.getOrElse(-1)}"
case class Accumulator(signals: Vector[Distress], totalNests: Int, nestStack: Vector[Int]):
  def distressAtCurrent(i: Option[Int]): Distress = Distress(nestStack.head, nestStack.size, i)
  def addSignal(o: Option[Int]): Accumulator      = this.copy(signals = signals :+ distressAtCurrent(o))
  def deeper: Accumulator    = Accumulator(signals, totalNests + 1, nestStack.prepended(totalNests + 1))
  def shallower: Accumulator = this.addSignal(None).copy(nestStack = nestStack.tail)

case class DistressSignal(left: Vector[Distress], right: Vector[Distress]):
  def inRightOrder: Boolean =

    @tailrec def loop(leftRemaining: Vector[Distress], rightRemaining: Vector[Distress]): Boolean =
      (leftRemaining.headOption, rightRemaining.headOption) match
        case (None, _)       => true
        case (Some(_), None) => false
        case (Some(l), Some(r)) =>
          if l.number.getOrElse(-1) < r.number.getOrElse(-1) then true
          else if l.number == r.number then loop(leftRemaining.tail, rightRemaining.tail)
          else false

    loop(left, right)

object DistressSignal:
  def fromVector2(v: Vector[String]): DistressSignal =
    val Vector(left, right) = v.map(parseDistressString)
    DistressSignal(left, right)
