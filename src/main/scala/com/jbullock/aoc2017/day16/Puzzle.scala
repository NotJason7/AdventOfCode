package com.jbullock.aoc2017.day16

import scala.io.Source
import scala.annotation.tailrec

object Puzzle:
  @main
  def solve(): Unit =
    val start = ('a' to 'p').mkString
    val input = Source.fromResource("aoc/2017/Day16/Input.txt").getLines.mkString.split(',').toVector
//    val start = ('a' to 'e').mkString
//    val input = "s1,x3/4,pe/b".split(',').toVector
    val instructions = input.map(_.asInstruction)
    val finish = start.followInstructions(instructions)
    println(s"Part 1: $finish")
    val cycleLength = danceUntilRepeat(start, instructions)
    val effectiveBillionthDance = 1000000000 % cycleLength
    val billionthFinish = nDances(start, effectiveBillionthDance, instructions)
    println(s"Part 2: $billionthFinish")

def nDances(start: String, n: Int, instructions: Vector[String => String]): String =
  if n == 0 then start else (1 to n).foldLeft(start)((s, _) => s.followInstructions(instructions))

def danceUntilRepeat(start: String, instructions: Vector[String => String]): Int =
  @tailrec
  def loop(current: String, n: Int): Int =
    if current == start then n else loop(current.followInstructions(instructions), n + 1)
  loop(start.followInstructions(instructions), 1)

extension(s: String)
  def spin(n: Int): String = if n >= 0 then s.takeRight(n) + s.dropRight(n) else s.drop(n) + s.take(n)
  def exchange(x: Int, y: Int): String = s.toVector.updated(x, s(y)).updated(y, s(x)).mkString
  def partner(a: Char, b: Char): String = s.toVector.updated(s.indexOf(a), b).updated(s.indexOf(b), a).mkString
  def asInstruction: String => String = s.head match
    case 's' => (n: String) => n.spin(s.tail.toInt)
    case 'x' => (n: String) =>
      val (x, y) = s.drop(1).replace("/","").splitAt(s.indexOf('/') - 1)
      n.exchange(x.toInt, y.toInt)
    case 'p' => (n :String) => n.partner(s(1), s(3))
    case _ => throw new RuntimeException(s"Unable to parse instruction ${s}")
  def followInstructions(instructions: Vector[String => String]): String = instructions.foldLeft(s)((x, f) => f(x))
