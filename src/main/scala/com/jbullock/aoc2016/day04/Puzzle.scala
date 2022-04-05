package com.jbullock.aoc2016.day04

import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: Vector[String] = Source.fromResource("aoc/2016/Day04/Input.txt").getLines.toVector
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
  val part2Answer = Puzzle.part2(input)
  println(s"Part2: $part2Answer")

object Puzzle:
  def part1(input: Vector[String]): Int =
    input.map(Room.fromString).filter(_.isValidRoom).map(_.roomNumber).sum
  def part2(input: Vector[String]): Int =
    input.map(Room.fromString).filter(_.isValidRoom).filter(_.decryptName == "northpole object storage ").head.roomNumber

extension(c: Char)
  def caeserShiftLowerCase(n: Int): Char =
    val lowerCaseStart = 'a'.toInt
    ((c.toInt - lowerCaseStart + n) % 26 + lowerCaseStart).toChar

case class Room(cipherText: String, roomNumber: Int, checkSum: String)
object Room:
  def fromString(s: String): Room =
    val cipherText = s.take(s.length - 10)
    val roomNumber = s.takeRight(10).take(3).toInt
    val checkSum = s.takeRight(6).take(5)
    Room(cipherText, roomNumber, checkSum)
extension(r: Room)
  def constructCheckSum: String = r.cipherText
      .filterNot(_ == '-').groupMapReduce(identity)(_ => 1)(_ + _)
      .toVector.sortBy{case (char, frequency) => (-frequency, char)}
      .take(5).map(_._1).mkString
  def isValidRoom: Boolean =
    r.constructCheckSum == r.checkSum
  def decryptName: String =
    r.cipherText.map{ case char: Char => char match
      case '-' => ' '
      case _ => char.caeserShiftLowerCase(r.roomNumber)
    }
