package com.jbullock.aoc2017.day06

import scala.io.Source
import annotation.tailrec

@main
def solvePuzzle(): Unit =
  println(s"Part 1: ${Puzzle.part1}")
  println(s"Part 2: ${Puzzle.part2}")

object Puzzle:
  val input: String    = Source.fromResource("aoc/2017/Day06/Input.txt").getLines.mkString
  val loop: MemoryLoop = MemoryLoop.fromMemory(Memory(input.split("\\t").toVector.map(_.toInt)))
  def part1: Int       = loop.steps.size
  def part2: Int       = loop.steps.size - loop.steps.indexOf(loop.repeat)

case class Memory(banks: Vector[Int]):
  val maxBank: Int = banks.indexOf(banks.max)
  val blocks: Int  = banks.max
  def redistributeMaxBank: Memory = Memory {
    (banks.updated(maxBank, 0) ++ Vector.fill(maxBank + 1)(0) ++ Vector.fill(blocks)(1) //original, left pad, fill
      ++ Vector.fill(banks.size - (maxBank + 1 + blocks) % banks.size)(0))              //right pad
      .grouped(banks.size)
      .toVector
      .transpose
      .map(_.sum)
  }

case class MemoryLoop(repeat: Memory, steps: Vector[Memory])
object MemoryLoop:
  def fromMemory(start: Memory): MemoryLoop =
    @tailrec
    def loop(memory: Memory, seen: Vector[Memory]): MemoryLoop =
      if seen.contains(memory) then MemoryLoop(memory, seen)
      else loop(memory.redistributeMaxBank, seen :+ memory)
    loop(start, Vector.empty[Memory])
