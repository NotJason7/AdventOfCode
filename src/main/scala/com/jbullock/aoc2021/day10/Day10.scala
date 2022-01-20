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

  val openers = Set('{','[','<','(')
  val closers = Set('}',']','>',')')
  val bracketPairs = Map(
    '{' -> '}',
    '}' -> '{',
    '[' -> ']',
    ']' -> '[',
    '<' -> '>',
    '>' -> '<',
    '(' -> ')',
    ')' -> '(',
  )
  val syntaxScore = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )
  val syntaxScorePart2 = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )

  def part1(): Unit =
    val charLists = input.map(_.toList)
    val errors = charLists.map(checkSyntax(_)._1)
    val errorScore = errors.map(syntaxScore.getOrElse(_, 0)).sum
    println(s"Part one: $errorScore")

  def part2(): Unit =
    val charLists = input.map(_.toList)
    val incompleteListValues = charLists
      .map(checkSyntax(_))
      .filter(_._1 == ' ')
      .map(_._2)
      .map(_.map(syntaxScorePart2.getOrElse(_,0)))
    println(incompleteListValues)
    val scores = incompleteListValues.map{
      values => values.foldLeft(0L)((a, x) => (a * 5) + x)
    }.sorted
    println(scores)
    val winner = scores((scores.length/2))
    println(winner)
    
  def checkSyntax(chars: List[Char]): (Char, List[Char]) =
    val noExpectation = ' '
    val emptyStack = List[Char]()
      
    @tailrec
    def loop(chars: List[Char], expected: Char, stack: List[Char]): (Char, List[Char]) =
      chars match
        case Nil =>
          (noExpectation, stack.prepended(expected).dropRight(1))
        case head :: tail =>
          if openers contains head then
            loop(tail, bracketPairs.getOrElse(head, noExpectation), stack.prepended(expected))
          else if closers contains head then
            if head == expected then
              loop(tail, stack.headOption.getOrElse(noExpectation), stack.drop(1))
            else
              (head, stack.dropRight(1))
          else
            (noExpectation, stack.dropRight(1))

    loop(chars, noExpectation, emptyStack)
