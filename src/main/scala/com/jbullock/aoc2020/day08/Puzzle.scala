package com.jbullock.aoc2020.day08

@main def solvePuzzle(): Unit =
  val input                            = scala.io.Source.fromResource("aoc/2020/Day08/Input.txt").getLines.toVector
  val instructions                     = input.flatMap(Instruction.fromString)
  val start                            = BootState.from(instructions)
  val valueAtFirstDuplicateInstruction = start.followInstructionsUntilDuplicateOrTerminate.valueAtDuplicate.get
  println(s"Part 1: $valueAtFirstDuplicateInstruction")
  val swapIndexes = instructions.zipWithIndex
    .filter { case (instruction, index) =>
      instruction.isInstanceOf[NoOperation] || instruction.isInstanceOf[Jump]
    }
    .map(_._2)
  val possibleInstructions = swapIndexes
    .map(swapIndex => instructions.updated(swapIndex, Instruction.swap(instructions(swapIndex))))
    .map(swappedInstructions => BootState.from(swappedInstructions).followInstructionsUntilDuplicateOrTerminate)
    .find(_.valueAtTermination.isDefined)
    .flatMap(_.valueAtTermination)
  println(s"Part 2: ${possibleInstructions.get}")

trait Instruction:
  def next(bootState: BootState): BootState
case class NoOperation(value: Int) extends Instruction:
  override def next(bootState: BootState): BootState = bootState.addToVisited.addToIndex(1)
case class Accumulate(value: Int) extends Instruction:
  override def next(bootState: BootState): BootState = bootState.addToVisited.addToIndex(1).addToAccumulator(value)
case class Jump(value: Int) extends Instruction:
  override def next(bootState: BootState): BootState = bootState.addToVisited.addToIndex(value)

object Instruction:
  def swap(i: Instruction): Instruction = i match
    case noOperation: NoOperation => Jump(noOperation.value)
    case jump: Jump               => NoOperation(jump.value)
    case _                        => i
  def fromString(s: String): Option[Instruction] = s match
    case s"$operationString $valueString" =>
      operationString match
        case "nop" => Some(NoOperation(valueString.toInt))
        case "acc" => Some(Accumulate(valueString.toInt))
        case "jmp" => Some(Jump(valueString.toInt))
    case _ => None

case class TerminationValues(valueAtDuplicate: Option[Int], valueAtTermination: Option[Int])

case class BootState(
    instructions: Vector[Instruction],
    instructionIndex: Int,
    accumulator: Int,
    instructionsVisited: Set[Int]
):
  def addToVisited: BootState             = this.copy(instructionsVisited = instructionsVisited + instructionIndex)
  def addToIndex(n: Int): BootState       = this.copy(instructionIndex = instructionIndex + n)
  def addToAccumulator(n: Int): BootState = this.copy(accumulator = accumulator + n)
  private def currentInstruction: Option[Instruction] =
    if instructions.indices.contains(instructionIndex) then Some(instructions(instructionIndex)) else None
  def followInstructionsUntilDuplicateOrTerminate: TerminationValues =
    if instructionsVisited.contains(instructionIndex) then TerminationValues(Some(accumulator), None)
    else
      currentInstruction match
        case Some(instruction) => instruction.next(this).followInstructionsUntilDuplicateOrTerminate
        case None              => TerminationValues(None, Some(accumulator))
object BootState:
  def from(instructions: Vector[Instruction]): BootState = BootState(instructions, 0, 0, Set.empty[Int])
