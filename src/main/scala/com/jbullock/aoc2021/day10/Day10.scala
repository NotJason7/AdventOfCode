package com.jbullock.aoc2021.day10

import scala.annotation.tailrec
import scala.io.Source

@main
def runDay10(): Unit =
  Day10.part1()
  Day10.part2()


object Day10:
  val input = Source
    .fromResource("2021/Day10Input.txt")
    .getLines
    .toList
    .map(_.toList)

  val openers = Map(
    '{' -> '}',
    '[' -> ']',
    '<' -> '>',
    '(' -> ')'
  )
  val closers = Map(
    '}' -> '{',
    ']' -> '[',
    '>' -> '<',
    ')' -> '('
  )
  val syntaxScore = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )
  val syntaxPoints = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )

  def part1(): Unit =
    val processedInput = input.map(checkSyntax(_))
    val invalidInput = processedInput.collect { case Left(charError) => charError }
    val invalidSyntaxScore = invalidInput.map(syntaxScore(_)).sum
    println(s"Part one: $invalidSyntaxScore")

  def part2(): Unit =
    val processedInput = input.map(checkSyntax(_))
    val finishers = processedInput.collect { case Right(finisher) => finisher }
    val points = finishers.map(_.map(syntaxPoints(_)))
    val scores = points.map{
      values => values.foldLeft(0L)((a, x) => (a * 5) + x)
    }.sorted
    val winner = scores(scores.length/2)
    println(s"Part two: $winner")
    
  def checkSyntax(chars: List[Char]): Either[Char, List[Char]] =
      
    @tailrec
    def loop(chars: List[Char], stack: List[Char]): Either[Char, List[Char]] =
      chars match
        case Nil =>
          Right(stack)
        case head :: tail =>
          if openers.keys.toSet.contains(head) then
            loop(tail, openers(head) :: stack)
          else if closers.keys.toSet.contains(head) then stack.headOption match
            case Some(expected) =>
              if head == expected then loop(tail, stack.drop(1))
              else Left(head)
            case None =>
              Right(stack)
          else Right(stack)

    loop(chars, List())
