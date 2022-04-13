package com.jbullock.aoc2016.day05

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val input: Vector[String] = Source.fromResource("aoc/2016/Day05/Input.txt").getLines.toVector
  val part1Answer = Puzzle.part1(input)
  println(s"Part1: $part1Answer")
  val part2Answer = Puzzle.part2(input)
  println(s"Part2: $part2Answer")

object Puzzle:
  def part1(input: Vector[String]): String =
    getPassword(input.head)
  def part2(input: Vector[String]): String =
    getHarderPassword(input.head)

def getPassword(s: String): String =
  @tailrec
  def loop(s: String, password: String, i: Int): String =
    if password.length == 8 then password
    else
      val (c, newI, _) = passwordCharacter(s, i)
      loop(s, password + c, newI+1)
  loop(s, "", 0)

def getHarderPassword(s: String): String =
  @tailrec
  def loop(s: String, password: Map[Int, String], i: Int): String =
    if password.keys.size == 8 then
      (0 to 7).toVector.map(password).mkString
    else
      val (index, newI, char) = passwordCharacter(s, i)
      val newPassword =
        if ('0' to '7').contains(index) && !password.contains(index.asDigit) then password + (index.asDigit -> char.toString) else password
      loop(s, newPassword, newI+1)
  loop(s, Map.empty[Int, String], 0)

@tailrec
def passwordCharacter(s: String, i: Int): (Char, Int, Char) =
  val text = s ++ i.toString
  val hashed = md5(text)
  if hashed.take(5) == "00000" then (hashed(5), i, hashed(6))
  else passwordCharacter(s, i+1)

def md5(s: String): String =
  import java.math.BigInteger
  import java.security.MessageDigest
  val digest = MessageDigest.getInstance("MD5").digest(s.getBytes)
  val hashedPassword = new BigInteger(1, digest).toString(16).trim
  def prependWithZeros(pwd: String): String =
    "%1$32s".format(pwd).replace(' ', '0')
  prependWithZeros(hashedPassword)

