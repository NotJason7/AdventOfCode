package com.jbullock.aoc2021.day10

import scala.annotation.tailrec
import scala.io.Source

@main
def runDay10(): Unit =
  Day10.part1()
//   Day10.part2()

object Day10:
  val input = Source
    .fromResource("2021/Day10TestInput.txt")
    .getLines
    .toList

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

  def part1(): Unit =
    val charLists = input.map(_.toList)
    val list0 = charLists(2)
    println(list0)
    val test = checkSyntax(list0)
    println(test)
    
  def checkSyntax(charList: List[Char]): Char =
      
    @tailrec
    def loop(charList: List[Char], openedCharMap: Map[Char, Int]): Char =
      charList match
        case Nil =>
          println("somehowgotanil")
          ' '
        case head :: tail =>
          println(s"head is $head")
          val newCharMap = updateMap(head, openedCharMap)
          newCharMap match
            case None => head
            case Some(map) =>
              loop(tail, map)
    
    def updateMap(char: Char, openedCharMap: Map[Char, Int]): Option[Map[Char, Int]] =
      char match
        case '{' | '[' | '<' | '(' =>
          println(s"char $char is an opener")
          val newValue = openedCharMap.getOrElse(char, 0) + 1
          val newMap = openedCharMap + (char -> newValue)
          println(newMap)
          Some(openedCharMap + (char -> newValue))
        case '}' | ']' | '>' | ')' =>
          println(s"char $char is a closer")
          val openingChar = bracketPairs.get(char)
          openingChar match
            case None => None
            case Some(opener) =>
              val newValue = openedCharMap.getOrElse(opener, 0) - 1
              if newValue == -1 then
                println(s"char $char was BAD")
                None
              else  
                val newMap = openedCharMap + (opener -> newValue)
                println(newMap)
                Some(newMap)
    
    loop(charList, Map())
          
