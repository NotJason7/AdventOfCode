package com.jbullock.aoc2017.day15

import scala.annotation.tailrec

object Puzzle:
  @main
  def solve(): Unit =
    val part1 = countMatches(State(516, 190, Mode.Normal), 40000000)
    val part2 = countMatches(State(516, 190, Mode.Picky), 5000000)
    println(s"Part 1: ${part1}\nPart 2: ${part2}")

extension(l: Long)
  def toPaddedBinaryString(n: Int): String = l.toBinaryString.reverse.padTo(n, 0).reverse.mkString

enum Mode:
  case Normal, Picky

case class State(a: Long, b: Long, mode: Mode):
  val (aFactor, bFactor, divisor) = (16807, 48271, Int.MaxValue)

  def lowest16BitsMatch: Boolean = a.toPaddedBinaryString(32).takeRight(16) == b.toPaddedBinaryString(32).takeRight(16)

  def nextValue(current: Long, factor: Long): Long = (current * factor) % divisor

  def nextValueDivisibleByN(current: Long, factor: Long, n: Int): Long =
    @tailrec
    def loop(current: Long): Long =
      if current % n == 0 then current else loop(nextValue(current, factor))

    loop(nextValue(current, factor))

  def next: State = mode match
    case Mode.Normal => State(nextValue(a, aFactor), nextValue(b, bFactor), mode)
    case Mode.Picky  => State(nextValueDivisibleByN(a, aFactor, 4), nextValueDivisibleByN(b, bFactor, 8), mode)

def countMatches(state: State, n: Int): Int =
  @tailrec
  def loop(state: State, n: Int, total: Int): Int =
    if n == 0 then total else loop(state.next, n-1, if state.lowest16BitsMatch then total + 1 else total)

  loop(state, n, 0)
