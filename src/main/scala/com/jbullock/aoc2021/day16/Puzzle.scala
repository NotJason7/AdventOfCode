package com.jbullock.aoc2021.day16

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: List[String] = Source
    .fromResource("aoc/2021/Day16/Example.txt")
    .getLines
    .toList
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
//  val part2Answer = Puzzle.part2(input)
//  println(s"Part2: $part2Answer")



extension(s: String)
  def binaryToInteger: Int =
    Integer.parseInt(s, 2)

def hexToBinaryString(char: Char): String =
  val hexToBinaryMap = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111",
  )
  hexToBinaryMap(char)

case class Header(version: Int, typeId: Int)
object Header:
  def fromHeaderString(s: String): Header =
    val version = s.take(3).binaryToInteger
    val typeId = s.slice(from=3, until=6).binaryToInteger
    Header(version, typeId)

case class Packet(header: Header, value: Int | List[Packet])
object Packet:
  def fromHexString(s: String): Packet =
    fromBinaryString(s.toList.map(hexToBinaryString).mkString)
  def fromBinaryString(packet: String): Packet =
    val header = Header.fromHeaderString(packet.take(6))
    val value = parseValue(header, packet.drop(6))
    Packet(header, value)
//    header match
//      case Header(_, 4) => Packet(header, decodeLiteral(value))
//      case Header(version, typeId) => Packet(header, 0)
//        val lengthTypeId = value.take(1)
//        if lengthTypeId == "0" then
//          val length = value.take(15).binaryToInteger
//        else
//          val subPacketCount = value.take(11).binaryToInteger
  def parseValue(header: Header, value: String): Int | List[Packet] =
    header match
      case Header(version, 4) => decodeLiteral(value)
      case Header(version, typeId) => 0


  def subPacketsFromLength(s: String): List[Packet] = ???

  def decodeLiteral(s: String): Int =
    val bits = s.sliding(5, 5).toList

    @tailrec
    def loop(literal: String, bits: List[String]): String =
      if bits.isEmpty then literal
      else
        val nextBit = bits.head
        val updateLiteral = literal + nextBit.tail
        if nextBit.head == '1' then loop(updateLiteral, bits.tail)
        else updateLiteral
    loop(bits.head.tail, bits.tail).binaryToInteger

//extension(t: Transmission)
////  def decode: String =
////    if t.typeId == 4 then t.literal.toString
//  def version: Int = t.packet.take(3).binaryToInteger
//  def typeId: Int = t.packet.slice(from=3, until=6).binaryToInteger
//  def lengthTypeId: Boolean = t.packet.slice(6, 7).toList.head == '1'
//  def totalBitLength: Int = t.packet.slice(7, 22).toList.mkString.binaryToInteger




object Puzzle {
  def part1(input: List[String]): String =
    val packets = input.map(Packet.fromHexString)
    println(packets)
    "1"
//    val test = packets.tail.head
//    println(test)
//    println(test.version)
//    println(test.typeId)
//    println(test.literal)
//    println(test.lengthTypeId)
//    "1"

  def part2(input: List[String]): String = ???
}
