package com.jbullock.aoc2020.day05

@main def solvePuzzle(): Unit =
  val input   = scala.io.Source.fromResource("aoc/2020/Day05/Input.txt").getLines.toVector
  val passIds = input.map(BoardingPass.fromString).map(_.id)
  println(s"Part 1: ${passIds.max}")
  val myId = (passIds.min to passIds.max)
    .find(id => !passIds.contains(id) && passIds.contains(id + 1) && passIds.contains(id - 1))
    .get
  println(s"Part 2: $myId")

case class BoardingPass(row: Int, column: Int):
  val id: Int = row * 8 + column
object BoardingPass:
  def fromString(s: String): BoardingPass =
    val row    = Integer.parseInt(s.take(7).replace('B', '1').replace('F', '0'), 2)
    val column = Integer.parseInt(s.takeRight(3).replace('R', '1').replace('L', '0'), 2)
    BoardingPass(row, column)
