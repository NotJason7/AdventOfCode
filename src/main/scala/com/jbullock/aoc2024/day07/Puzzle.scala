package com.jbullock.aoc2024.day07

@main def solvePuzzle(): Unit =
  val input     = scala.io.Source.fromResource("aoc/2024/Day07/Input.txt").getLines.toSeq
  val equations = input.flatMap(Equation.fromString)
  val part1     = equations.filter(_.canBeTrue(false)).map(_.result).sum
  println(s"Part 1: $part1")
  val part2 = equations.filter(_.canBeTrue(true)).map(_.result).sum
  println(s"Part 2: $part2")

case class Equation(result: BigInt, values: Seq[BigInt]):
  def canBeTrue(useConcatenation: Boolean): Boolean = values.tail
    .foldLeft(Seq(values.head))((possibleValues: Seq[BigInt], nextValue: BigInt) =>
      possibleValues.flatMap { possibleValue =>
        val multiplication = possibleValue * nextValue
        val addition       = possibleValue + nextValue
        val concatenation  = BigInt(s"$possibleValue$nextValue")
        val nextPossibleValues = Seq(multiplication, addition, concatenation).filterNot(i =>
          if !useConcatenation then i == concatenation else false
        )
        nextPossibleValues.filter(_ <= result)
      }
    )
    .contains(result)

object Equation:
  def fromString(s: String): Option[Equation] = s match
    case s"$resultString: $valuesString" =>
      val result = BigInt.apply(resultString)
      val values = valuesString.split(' ').toSeq.map(i => BigInt(i.toInt))
      Some(Equation(result, values))
    case _ => None
