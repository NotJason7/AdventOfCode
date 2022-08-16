package com.jbullock.aoc2017.day13

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit = Puzzle

object Puzzle:
  given firewall: Vector[Layer] = Source.fromResource("aoc/2017/Day13/Input.txt").getLines.toVector.map(Layer.fromString)
//  given firewall: Vector[Layer] = Vector("0: 3","1: 2","4: 4","6: 4").map(Layer.fromString)
  val part1: State = State(0, 0).steps(firewall.takeRight(1).head.depth + 1)
  println(s"Part 1: $part1")
  val sieve: Int => Boolean = firewall.foldLeft((x: Int) => true)((f, layer) => (x: Int) => f(x) & (x % layer.depth
    * 2 == 0))
  println(sieve(1))

case class State(picoseconds: Int, severity: Int)(using firewall: Vector[Layer]):
  def step: State = firewall.find(layer => layer.depth == picoseconds && layer.positionAt(picoseconds) == 0) match
    case Some(layer) => State(picoseconds + 1, severity + layer.severity)
    case _ => this.copy(picoseconds = picoseconds + 1)
  def steps(n: Int): State =
    @tailrec
    def loop(current: State, remaining: Int): State =
      if remaining == 0 then current
      else loop(current.step, remaining - 1)
    loop(this, n)

case class Layer(depth: Int, range: Int):
  val severity: Int = depth * range
  def positionAt(t: Int): Int =
    val search: Vector[Int] = ((0 until range) ++ (range - 2 to 1 by -1)).toVector
    LazyList.continually(search).to(LazyList).flatten.drop(t).head
object Layer:
  def fromString(s: String): Layer =
    "\\d+".r.findAllIn(s).toVector.map(_.toInt) match
      case Vector(depth: Int, range: Int) => Layer(depth, range)
      case _ => throw new RuntimeException(s"Unable to create Layer from ${s}")
