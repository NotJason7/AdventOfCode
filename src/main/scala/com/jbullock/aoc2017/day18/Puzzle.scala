package com.jbullock.aoc2017.day18

import scala.annotation.tailrec
import scala.io.Source

object Puzzle:
  @main def solve(): Unit =
    given instructions: Vector[String] = Source.fromResource("aoc/2017/Day18/Input.txt").getLines.toVector
    val duetOutput = duet(Program.fromId(0), Program.fromId(1))
    println(duetOutput)

@tailrec def duet(p0: Program, p1: Program)(using instructions: Vector[String]): BigInt =
  val nextP0 = p0.clearSent.copy(received = p1.sent).followUntilWait
  val nextP1 = p1.clearSent.copy(received = p0.sent).followUntilWait
  if p0.totalSent == nextP0.totalSent && p1.totalSent == nextP1.totalSent then nextP1.totalSent
  else duet(nextP0, nextP1)

case class Program(
    id: Int,
    sent: Vector[BigInt],
    totalSent: BigInt,
    received: Vector[BigInt],
    registers: Map[String, BigInt],
    instruction: Int
)(using instructions: Vector[String]):
  def xGet(x: String): BigInt = registers.getOrElse(x, 0)
  def yGet(y: String): BigInt = y.toIntOption match
    case Some(n) => BigInt(n)
    case None    => xGet(y)
  def snd(x: String): Program = this.copy(
    sent = sent :+ xGet(x),
    totalSent = totalSent + 1,
    instruction = instruction + 1)
  def set(x: String, y: BigInt): Program =this.copy(
    registers = registers.updated(x, y),
    instruction = instruction + 1)
  def add(x: String, y: BigInt): Program =this.copy(
    registers = registers.updated(x, (xGet(x) + y)),
    instruction = instruction + 1)
  def mul(x: String, y: BigInt): Program =this.copy(
    registers = registers.updated(x, (xGet(x) * y)),
    instruction = instruction + 1)
  def mod(x: String, y: BigInt): Program =this.copy(
    registers = registers.updated(x, (xGet(x) % y)),
    instruction = instruction + 1)
  def rcv(x: String): Program = received.headOption match
    case Some(i) => this.copy(
      registers = registers.updated(x, i),
      received = received.drop(1),
      instruction = instruction + 1)
    case _ => this
  def jgz(x: BigInt, y: BigInt): Program =
    if x > 0 then this.copy(instruction = (instruction + y.toInt))
    else this.copy(instruction = instruction + 1)
  def followInstruction: Program =
    val i = instructions(instruction)
    val x = i(4).toString
    val y = yGet(i.drop(6))
    i.take(3) match
      case "snd" => this.snd(x)
      case "set" => this.set(x, y)
      case "add" => this.add(x, y)
      case "mul" => this.mul(x, y)
      case "mod" => this.mod(x, y)
      case "rcv" => this.rcv(x)
      case "jgz" => this.jgz(yGet(x), y)
      case _ => throw new RuntimeException(s"Unexpected instruction found: $i")
  @tailrec final def followUntilWait: Program =
    if instructions(instruction).take(3) == "rcv" && received.isEmpty then this
    else this.followInstruction.followUntilWait
  def clearSent: Program =
    this.copy(sent = Vector.empty[BigInt])
object Program:
  def fromId(id: Int)(using instructions: Vector[String]): Program =
    Program(id, Vector.empty[BigInt], 0, Vector.empty[BigInt], Map("p" -> BigInt(id)), 0)

