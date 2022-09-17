package com.jbullock.aoc2017.day18

import scala.annotation.tailrec
import scala.io.Source

object Puzzle:
  @main def solve(): Unit =
    val instructions = Source.fromResource("aoc/2017/Day18/Input.txt").getLines.toVector
//    val instructions = Vector("set a 1","add a 2","mul a a","mod a 5","snd a","set a 0","rcv a","jgz a -1","set a 1","jgz a -2")
    val start = Assembly(None, None, Map.empty[String, Long], instructions, 0)
    val received = start.followInstructionsPart1()
    println(s"Part 1: $received")
    val program0 = Assembly(None, None, Map("p" -> 0L), instructions, 0)
    val program1 = Assembly(None, None, Map("p" -> 1L), instructions, 0)

//    val end = i.asInstruction(start)
//    println(end)

//snd X plays a sound with a frequency equal to the value of X.
//set X Y sets register X to the value of Y.
//add X Y increases register X by the value of Y.
//mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
//mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
//rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
//jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
case class Duet(program0: Assembly, program1: Assembly)

case class Assembly(
    sent: Option[Long],
    received: Option[Long],
    registers: Map[String, Long],
    instructions: Vector[String],
    current: Int
):
  def xGet(x: String): Long = registers.getOrElse(x, 0L)
  def yGet(y: String): Long = y.toLongOption match
    case Some(n) => n
    case None    => xGet(y)
  def snd(x: String): Assembly =
    this.copy(sent = Some(xGet(x)), current = current + 1)
  def set(x: String, y: String): Assembly =
    this.copy(registers = registers.updated(x, yGet(y)), current = current + 1)
  def add(x: String, y: String): Assembly = this.copy(
    registers = registers.updated(x, (xGet(x) + yGet(y))),
    current = current + 1
  )
  def mul(x: String, y: String): Assembly = this.copy(
    registers = registers.updated(x, (xGet(x) * yGet(y))),
    current = current + 1
  )
  def mod(x: String, y: String): Assembly = this.copy(
    registers = registers.updated(x, (xGet(x) % yGet(y))),
    current = current + 1
  )
  def rcv(x: String): Assembly =
    if xGet(x) > 0 && sent.getOrElse("") != "" then
      this.copy(received = Some(sent.get))
    else this.copy(current = current + 1)
  def jgz(x: String, y: String): Assembly =
    if xGet(x) > 0 then this.copy(current = (current + yGet(y).toInt))
    else this.copy(current = current + 1)
  def followInstruction: Assembly =
    val instruction = instructions(current.toInt)
    val x = instruction(4).toString
    val y = instruction.drop(6)
    instruction.take(3) match
      case "snd" => this.snd(x)
      case "set" => this.set(x, y)
      case "add" => this.add(x, y)
      case "mul" => this.mul(x, y)
      case "mod" => this.mod(x, y)
      case "rcv" => this.rcv(x)
      case "jgz" => this.jgz(x, y)
      case _ =>
        throw new RuntimeException(
          s"Unexpected instruction found: $instruction"
        )
  @tailrec final def followInstructionsPart1(): String =
    val next = this.followInstruction
    next.received match
      case Some(rcv) => rcv.toString
      case _ => next.followInstructionsPart1()
