package com.jbullock.aoc2017.day17

object Puzzle:
  @main
  def solve(): Unit =
    val cycle = 345
    val spinlock = (1 to 2017).toVector.foldLeft(Vector(0))((a, x) => a.insertModN(x, cycle))
    println(s"Part 1: ${spinlock(spinlock.indexOf(spinlock.max) + 1 % spinlock.length)}")
    val part2 = (1 to 50000000).toVector.foldLeft(State(0, 0, 0, 1))((s, _) => s.next(cycle))
    println(s"Part 2: ${part2.afterZero}")

case class State(zero: Int, afterZero: Int, current: Int, length: Int):
  def next(cycle: Int): State =
    val nextLength = length + 1
    val nextCurrent = (current + cycle) % length + 1
    val nextZero = if nextCurrent <= zero then zero + 1 else zero
    val nextAfterZero = if nextCurrent == nextZero + 1 then length else afterZero
    State(nextZero, nextAfterZero, nextCurrent, nextLength)

extension(v: Vector[Int])
  def insertModN(x: Int, n: Int): Vector[Int] =
    if v.length == 1 then Vector(0, 1) else
      val (a, b) = v.splitAt((v.indexOf(v.max) + n ) % v.length + 1)
      a ++ Vector(x) ++ b
