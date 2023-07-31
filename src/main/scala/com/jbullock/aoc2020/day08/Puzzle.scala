package com.jbullock.aoc2020.day08

@main def solvePuzzle(): Unit =
  val input                            = scala.io.Source.fromResource("aoc/2020/Day08/Input.txt").getLines.toVector
  val instructions                     = input.flatMap(Instruction.fromString)
  val start                            = BootState.from(instructions)
  val valueAtFirstDuplicateInstruction = start.followInstructions.valueAtDuplicate
  println(s"Part 1: ${valueAtFirstDuplicateInstruction.get}")
  val swapIndexes = instructions.zipWithIndex
    .filter { case (instruction, _) => instruction.isInstanceOf[NoOperation] || instruction.isInstanceOf[Jump] }
    .map { case (_, index) => index }
  val possibleInstructions = swapIndexes.view
    .map(swapIndex => instructions.updated(swapIndex, Instruction.swap(instructions(swapIndex))))
    .map(swappedInstructions => BootState.from(swappedInstructions).followInstructions)
    .find(_.valueAtTermination.isDefined)
    .flatMap(_.valueAtTermination)
  println(s"Part 2: ${possibleInstructions.get}")

trait Instruction:
  def follow(bootState: BootState): BootState
case class NoOperation(value: Int) extends Instruction:
  override def follow(bootState: BootState): BootState = bootState.addToIndex(1)
case class Accumulate(value: Int) extends Instruction:
  override def follow(bootState: BootState): BootState = bootState.addToIndex(1).addToAccumulator(value)
case class Jump(value: Int) extends Instruction:
  override def follow(bootState: BootState): BootState = bootState.addToIndex(value)

object Instruction:
  def swap(instruction: Instruction): Instruction = instruction match
    case noOperation: NoOperation => Jump(noOperation.value)
    case jump: Jump               => NoOperation(jump.value)
    case _                        => instruction
  def fromString(string: String): Option[Instruction] = string match
    case s"$operationString $valueString" =>
      operationString match
        case "nop" => Some(NoOperation(valueString.toInt))
        case "acc" => Some(Accumulate(valueString.toInt))
        case "jmp" => Some(Jump(valueString.toInt))
    case _ => None

case class FinalValue(valueAtDuplicate: Option[Int], valueAtTermination: Option[Int])

case class BootState(instructions: Vector[Instruction], index: Int, accumulator: Int, visited: Set[Int]):
  private def currentInstruction: Option[Instruction] =
    if instructions.isDefinedAt(index) then Some(instructions(index)) else None
  private def markVisited: BootState      = this.copy(visited = visited + index)
  def addToIndex(n: Int): BootState       = this.markVisited.copy(index = index + n)
  def addToAccumulator(n: Int): BootState = this.copy(accumulator = accumulator + n)
  def followInstructions: FinalValue = if visited.contains(index) then FinalValue(Some(accumulator), None)
  else
    currentInstruction match
      case Some(instruction) => instruction.follow(this).followInstructions
      case None              => FinalValue(None, Some(accumulator))
object BootState:
  def from(instructions: Vector[Instruction]): BootState = BootState(instructions, 0, 0, Set.empty[Int])
