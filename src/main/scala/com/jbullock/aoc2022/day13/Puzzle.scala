package com.jbullock.aoc2022.day13

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input =
    io.Source.fromResource("aoc/2022/Day13/Sample.txt").getLines.toVector.filterNot(_.isBlank).grouped(2).toVector
  val signals = input.map(DistressSignal.fromVector2)
//  val test    = signals(4).left.groupMap(_.list)(_.number).values.toVector
//  println(test)
//  println(signals(4))
//  val s = signals(6)
//  val l = s.left.groupMap(_.list)(_.number).values.toVector
//  val r = s.right.groupMap(_.list)(_.number).values.toVector
//  println(s"$l\n$r")
//  val x = l.zip(r).map { case (left, right) =>
//    left.zip(right).forall { (l, r) =>
//      (l, r) match
//        case (None, _)          => true
//        case (Some(_), None)    => false
//        case (Some(x), Some(y)) => x <= y
//    }
//  }
//  println(x)
  val rightOrder = signals.map(_.inRightOrder)
//expected - T T F T F T F F
//actual   - T F F T F T F F
  println(rightOrder)

def parseDistressString(s: String): Vector[Distress] =
  @tailrec def loop(remaining: String, accumulator: Accumulator): Vector[Distress] = remaining.headOption match
    case None      => accumulator.signals
    case Some('[') => loop(remaining.tail, accumulator.deeper)
    case Some(']') => loop(remaining.tail, accumulator.shallower)
    case Some(',') => loop(remaining.tail, accumulator)
    case Some(_) =>
      val intString     = remaining.takeWhile(c => !Set('[', ',', ']').contains(c))
      val nextRemaining = remaining.drop(intString.length)
      loop(nextRemaining, accumulator.addSignal(intString.toInt))

  loop(s, Accumulator(Vector.empty[Distress], 0, Vector(0)))

case class Distress(list: Int, level: Int, number: Option[Int])
case class Accumulator(signals: Vector[Distress], totalNests: Int, nestStack: Vector[Int]):
  def currentDistress(i: Option[Int]): Distress = Distress(nestStack.head, nestStack.size, i)
  def addSignal(i: Int): Accumulator            = this.copy(signals = signals :+ currentDistress(Some(i)))
  def deeper: Accumulator =
    Accumulator(signals :+ currentDistress(None), totalNests + 1, nestStack.prepended(totalNests + 1))
  def shallower: Accumulator = this.copy(totalNests = totalNests, nestStack.tail)

case class DistressSignal(left: Vector[Distress], right: Vector[Distress]):
  def inRightOrder: Boolean =

    @tailrec def loop(leftRemaining: Vector[Distress], rightRemaining: Vector[Distress]): Boolean =
      (leftRemaining.headOption, rightRemaining.headOption) match
        case (None, None)    => true
        case (None, Some(_)) => true
        case (Some(_), None) => false
        case (Some(l), Some(r)) =>
          if (l.list == r.list && l.number.getOrElse(-1) <= r.number.getOrElse(-1)) then
            loop(leftRemaining.tail, rightRemaining.tail)
          else false

    loop(left, right)

object DistressSignal:
  def fromVector2(v: Vector[String]): DistressSignal =
    val Vector(left, right) = v.map(parseDistressString)
    DistressSignal(left, right)
