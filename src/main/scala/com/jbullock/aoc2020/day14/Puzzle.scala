package com.jbullock.aoc2020.day14

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val commands = scala.io.Source.fromResource("aoc/2020/Day14/Input.txt").getLines.toVector.flatMap(Command.fromString)
  val start    = State(Mask(""), Map.empty[BigInt, String])
  val part1    = commands.foldLeft(start)((state, command) => state.applyMaskToRegisterValues(command)).registerSum
  println(s"Part 1: $part1")
  val part2 = commands.foldLeft(start)((state, command) => state.applyMaskToRegisterKeys(command)).registerSum
  println(s"Part 2: $part2")

case class State(currentMask: Mask, registers: Map[BigInt, String]):
  def registerSum: BigInt = registers.values.map(BigInt(_, 2)).sum
  def applyMaskToRegisterValues(command: Command): State = command match
    case mask @ Mask(_) => State(mask, registers)
    case MemoryOverride(register, value) =>
      val nextRegisters = registers + (register -> value.toNBitBinaryString(36).mask(currentMask))
      State(currentMask, nextRegisters)
  def applyMaskToRegisterKeys(command: Command): State = command match
    case mask @ Mask(_) => State(mask, registers)
    case MemoryOverride(register, value) =>
      val registersToSet = register.toNBitBinaryString(36).floatingMask(currentMask).floatingMaskToRegisters
      val nextRegisters = registersToSet.foldLeft(registers)((updatingRegisters, register) =>
        updatingRegisters + (register -> value.toNBitBinaryString(36))
      )
      State(currentMask, nextRegisters)

sealed trait Command
object Command:
  def fromString(s: String): Option[Command] = s match
    case s"mask = $maskValue"       => Some(Mask(maskValue))
    case s"mem[$register] = $value" => Some(MemoryOverride(BigInt(register.toInt), BigInt(value.toInt)))
case class Mask(value: String)                             extends Command
case class MemoryOverride(register: BigInt, value: BigInt) extends Command

extension (b: BigInt) def toNBitBinaryString(n: Int): String = ("0".repeat(n) + b.toString(2)).takeRight(n)

extension (s: String)
  def mask(m: Mask): String         = m.value.zipWithIndex.map((c, index) => if c == 'X' then s(index) else c).mkString
  def floatingMask(m: Mask): String = m.value.zipWithIndex.map((c, index) => if c == '0' then s(index) else c).mkString
  def floatingMaskToRegisters: Vector[BigInt] =
    @tailrec def loop(inProgress: Vector[String], xRemaining: Int): Vector[String] =
      if xRemaining == 0 then inProgress
      else loop(inProgress.flatMap(s => Vector(s.replaceFirst("X", "1"), s.replaceFirst("X", "0"))), xRemaining - 1)
    loop(Vector(s), s.count(_ == 'X')).map(BigInt(_, 2))
