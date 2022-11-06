package com.jbullock.aoc2017.day23

import scala.annotation.tailrec
import scala.io.Source

@main def solvePuzzle(): Unit =
  val input        = Source.fromResource("aoc/2017/Day23/Input.txt").getLines.toVector
  val instructions = Instructions(0, input)
  val registers    = ('a' to 'h').map(_.toString -> 0).toMap
  val start        = State(registers, instructions, Map.empty[String, Int])
  val end          = start.followInstructions
  println(s"Part 1: ${end.instructionCount("mul")}")
  val part2 = (108100 to 125100 by 17).count(n => !isPrime(n))
  println(s"Part 2: $part2")

def isPrime(n: Int): Boolean =
  if n <= 1 then false
  else if n == 2 then true
  else !(2 to Math.sqrt(n).toInt).exists(n % _ == 0)

case class Instructions(current: Int, items: Vector[String]):
  def next: Option[String] =
    if items.indices.contains(current) then Some(items(current)) else None
  def skip(n: Int): Instructions = this.copy(current = current + n)

case class State(registers: Map[String, Int], instructions: Instructions, instructionCount: Map[String, Int]):
  def updateRegisters(s: String, v: Int, instruction: String): State = this.copy(
    registers = registers.updated(s, v),
    instructions = instructions.skip(1),
    instructionCount = updateCount(instruction)
  )
  def updateCount(instruction: String): Map[String, Int] =
    instructionCount.updated(instruction, instructionCount.getOrElse(instruction, 0) + 1)
  def get(s: String): Int              = if registers.contains(s) then registers.getOrElse(s, 0) else s.toInt
  def set(x: String, y: String): State = updateRegisters(x, get(y), "set")
  def sub(x: String, y: String): State = updateRegisters(x, get(x) - get(y), "sub")
  def mul(x: String, y: String): State = updateRegisters(x, get(x) * get(y), "mul")
  def jnz(x: String, y: String): State =
    val skip = if get(x) == 0 then 1 else get(y)
    this.copy(instructions = instructions.skip(skip), instructionCount = updateCount("jnz"))
  def followInstruction: State = instructions.next match
    case None => this
    case Some(s: String) =>
      val Array(x, y) = s.drop(4).split(" ")
      s.take(3) match
        case "set" => set(x, y)
        case "sub" => sub(x, y)
        case "mul" => mul(x, y)
        case "jnz" => jnz(x, y)
        case _     => throw new RuntimeException(s"Unable to parse instruction $s")
  def followInstructions: State =
    @tailrec def loop(s: State): State =
      val i = s.instructions
      if i.items.indices.contains(i.current) then loop(s.followInstruction)
      else s
    loop(this)
