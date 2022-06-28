package com.jbullock.aoc2021.day17

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: List[String] = Source
    .fromResource("aoc/2021/Day17/Example.txt")
    .getLines
    .toList
  println(s"Part1: ${Puzzle.part1(input)}")

object Puzzle {
  def part1(input: List[String]): String =
//    val target = Area(Range(20, 30), Range(-10, -5))
//    val initialPosition = Position(0, 0)
//    val initialVelocity = Velocity(7,9)
//    val start = State(initialPosition, initialVelocity)
//    val t = Trajectory.fromState(start, target)
//    println(t)
    println(20.minXVelocity)
    println(30.maxXVelocity)
    println(-10.minYVelocity)
    println((0 to 10).map(n => (0 to n).toList.sum))
    "lol"

  def part2(input: List[String]): String = ???
}

def sumToN(n: Int): Int = (0 to n by n.sign).toVector.sum

extension(i: Int)
  def minXVelocity: Int =
    @tailrec
    def loop(n: Int, sumN: Int): Int =
      if sumN.abs >= i.abs then n
      else loop(n + i.sign, sumN + n + i.sign)
    loop(0, 0)

  def maxXVelocity: Int =
    @tailrec
    def loop(n: Int, sumN: Int): Int =
      if sumN.abs > i.abs then n
      else loop(n + i.sign, sumN + n + i.sign)
    loop(0, 0)

  def minYVelocity: Int =
    if i <= 0 then 0
    else
      @tailrec
      def loop(n: Int, sumN: Int): Int =
        if sumN >= i then n
        else loop(n + 1, sumN + n + 1)
      loop(0, 0)

//  def maxYVelocity: Int
//    if 17-05 >= 0 then 17-05
//    else


case class Area(xRange: Range, yRange: Range)

case class Position(x: Int, y: Int):
  def move(v: Velocity): Position = Position(x + v.x, y + v.y)
  def inArea(a: Area): Boolean =
    a.xRange.contains(x) && a.yRange.contains(y)
case class Velocity(x: Int, y: Int):
  def tick: Velocity = Velocity(x - x.sign, y-1)

case class State(position: Position, velocity: Velocity):
  def nextState: State = State(position.move(velocity), velocity.tick)

case class Trajectory(maxHeight: Int, hitTarget: Boolean)
object Trajectory:
  def fromState(s: State, a: Area): Trajectory =
    val maxHeight = s.position.y + (1 to s.velocity.y).toVector.sum
    def i = Iterator.unfold(s){ state =>
      println(state)
      val next = state.nextState
      if next.velocity.y < 0 && next.position.y < a.yRange.start then None
      else Some((next, next))
    }
    val hitTarget = i.exists(_.position.inArea(a))
    Trajectory(maxHeight, hitTarget)
