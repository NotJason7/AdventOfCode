package com.jbullock.aoc2017.day13

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =

  println(s"Part 1: ${Puzzle.part1}\nPart 2: ${Puzzle.part2}")

object Puzzle:
  given firewall: Vector[Layer] = Source.fromResource("aoc/2017/Day13/Input.txt").getLines.toVector.map(Layer.fromString)
  def sieve: Int => Boolean = firewall.foldLeft((x: Int) => true){ (f, layer) =>
    (x: Int) => f(x) && ((x + layer.depth) % ((layer.range-1) * 2) != 0)
  }
  val part1: Int = State(0, 0).steps(firewall.last.depth).severity
  val part2: Int = LazyList.from(0).filter(sieve(_)).head

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
    val search = (0 until range) ++ (range - 2 to 1 by -1)
    LazyList.continually(search).to(LazyList).flatten.drop(t).head
object Layer:
  def fromString(s: String): Layer = "\\d+".r.findAllIn(s).toVector.map(_.toInt) match
    case Vector(depth: Int, range: Int) => Layer(depth, range)
    case _ => throw new RuntimeException(s"Unable to create Layer from ${s}")
