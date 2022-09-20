package com.jbullock.aoc2017.day18

import scala.annotation.tailrec
import scala.io.Source

object Puzzle:
  @main def solve(): Unit =
    given instructions: Vector[String] = Source.fromResource("aoc/2017/Day18/Input.txt").getLines.toVector
//    given instructions = Vector("set a 1","add a 2","mul a a","mod a 5","snd a","set a 0","rcv a","jgz a -1","set a
//    1","jgz a -2")
//    given instructions: Vector[String] = Vector("snd 1","snd 2","snd p","rcv a","rcv b","rcv c","rcv d")
    val program0 = Program.fromId(0)
    val program1 = Program.fromId(1)
//    val program0 = Assembly(0, None, 0L, Vector.empty[BigInt], Map("p" -> 0L), 0)
//    val program1 = Assembly(1, None, 0L, Vector.empty[BigInt], Map("p" -> 1L), 0)
//    val start = Duet(program0, program1)
//    val end = start.runUntilTerminate
//    println(s"Part 2: ${end.program1.totalSent}")

//case class Duet(program0: Assembly, queue0: Vector[Long], program1: Assembly, queue1: Vector[Long])(using instructions: Vector[String]):
//  def next: Duet =
//    val stage0 = program0.followInstruction
//    val stage1 = program1.followInstruction
//    val next0 = stage0.receiveExternal(stage1.sent)
//    val next1 = stage1.receiveExternal(stage0.sent)
//    Duet(next0, next1)
//  @tailrec final def runUntilTerminate: Duet =
//    val next = this.next
//    if this == next then this
//    else next.runUntilTerminate


case class Program(
    id: Int,
    sent: Option[BigInt],
    received: Vector[BigInt],
    registers: Map[String, BigInt],
    instruction: Int
)(using instructions: Vector[String]):
  def xGet(x: String): BigInt = registers.getOrElse(x, 0)
  def yGet(y: String): BigInt = y.toIntOption match
    case Some(n) => BigInt(n)
    case None    => xGet(y)
  def snd(x: String): Program =
    this.copy(sent = Some(xGet(x)), instruction = instruction + 1)
  def set(x: String, y: String): Program = this.copy(
    registers = registers.updated(x, yGet(y)),
    instruction = instruction + 1)
  def add(x: String, y: String): Program = this.copy(
    registers = registers.updated(x, (xGet(x) + yGet(y))),
    instruction = instruction + 1)
  def mul(x: String, y: String): Program = this.copy(
    registers = registers.updated(x, (xGet(x) * yGet(y))),
    instruction = instruction + 1)
  def mod(x: String, y: String): Program = this.copy(
    registers = registers.updated(x, (xGet(x) % yGet(y))),
    instruction = instruction + 1)
  def rcv(x: String): Program = received.headOption match
    case Some(i) => this.copy(
      registers = registers.updated(x, i),
      received = received.drop(1),
      instruction = instruction + 1)
    case _ => this
  def jgz(x: String, y: String): Program =
    if xGet(x) > 0 then this.copy(instruction = (instruction + yGet(y).toInt))
    else this.copy(instruction = instruction + 1)
  def followInstruction: Program =
    val i = instructions(instruction)
    val x = i(4).toString
    val y = i.drop(6)
    i.take(3) match
      case "snd" => this.snd(x)
      case "set" => this.set(x, y)
      case "add" => this.add(x, y)
      case "mul" => this.mul(x, y)
      case "mod" => this.mod(x, y)
      case "rcv" => this.rcv(x)
      case "jgz" => this.jgz(x, y)
      case _ => throw new RuntimeException(s"Unexpected instruction found: $i")
  @tailrec final def followUntilWait: Program = instructions(instruction).take(3) match
    case "rcv" if received.isEmpty => this
    case _ => this.followInstruction.followUntilWait
object Program:
  def fromId(id: Int)(using instructions: Vector[String]): Program =
    Program(id, None, Vector.empty[BigInt], Map("p" -> BigInt(id)), 0)

