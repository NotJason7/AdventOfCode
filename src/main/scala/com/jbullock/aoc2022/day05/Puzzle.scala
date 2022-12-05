package com.jbullock.aoc2022.day05

@main def solvePuzzle(): Unit =
  val input = io.Source.fromResource("aoc/2022/Day05/Input.txt").getLines.toVector
  val start = Cargo(
    input
      .takeWhile(s => !s.startsWith(" 1"))
      .map(_.replace("    ", "-").removeAll("[] ").toVector)
      .map(_.map(c => if c == '-' then None else Some(c)))
      .transpose
      .map(_.flatten)
  )
  val instructions = input
    .dropWhile(s => !s.startsWith("move"))
    .map(s => "[0-9]+".r.findAllIn(s).toVector.map(_.toInt))
    .map(Instruction.fromVector3)
  val cm9000End = instructions.foldLeft(start)((cargo, instruction) => cargo.followInstruction(instruction, false))
  val part1     = cm9000End.stacks.map(_.head).mkString
  println(s"Part 1: $part1")
  val cm9001End = instructions.foldLeft(start)((cargo, instruction) => cargo.followInstruction(instruction, true))
  val part2     = cm9001End.stacks.map(_.head).mkString
  println(s"Part 2: $part2")

extension (s: String)
  def removeAll(cs: String): String = cs.toVector.foldLeft(s)((string, delete) => string.replace(delete.toString, ""))

case class Instruction(amount: Int, source: Int, target: Int)
object Instruction:
  def fromVector3(v: Vector[Int]): Instruction =
    if v.size != 3 then throw RuntimeException(s"Instruction vector must have size 3, you tried ${v.size}")
    else Instruction(v(0), v(1) - 1, v(2) - 1)

case class Cargo(stacks: Vector[Vector[Char]]):
  def followInstruction(i: Instruction, canMultiCratePickUp: Boolean): Cargo =
    val newSource = stacks(i.source).drop(i.amount)
    val taken     = stacks(i.source).take(i.amount)
    val moved     = if canMultiCratePickUp then taken else taken.reverse
    val newTarget = moved ++ stacks(i.target)
    Cargo(stacks.updated(i.source, newSource).updated(i.target, newTarget))
