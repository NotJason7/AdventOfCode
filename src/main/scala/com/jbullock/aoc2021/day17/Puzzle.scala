package com.jbullock.aoc2021.day17

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
    val target = Area(Range(20, 30), Range(-10, -5))
    //
//    val vx = ??? // vx >= area.xRange.end
//    val vy = ??? // vy >= area.yRange.start
    val initialPosition = Position(0, 0)
    val initialVelocity = Velocity(6,9)
    val start = State(initialPosition, initialVelocity)
    val t = Trajectory.fromState(start, target)
    "lol"

  def part2(input: List[String]): String = ???
}

extension(i: Int)
  def lagLimit: Int =
    def iterator = Iterator.unfold(i){ n =>
      if n == 0 then None
      else
    }

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
    def i = Iterator.unfold(s){ state =>
      println(state)
      val next = state.nextState
      if next.velocity.y < 0 && next.position.y < a.yRange.start then None
      else Some((next, next))
    }
    val maxHeight = i.maxBy(_.position.y).position.y
    val hitTarget = i.exists(_.position.inArea(a))
    Trajectory(maxHeight, hitTarget)
