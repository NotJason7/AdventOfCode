package com.jbullock.aoc2021.day04

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  Puzzle.part1()
  Puzzle.part2()

case class Symbol(value: Int, isMarked: Boolean)
extension(s: Symbol) {
  def mark: Symbol =
    Symbol(s.value, true)
}

case class Line(symbols: List[Symbol])
extension(l: Line) {
  def hasWon: Boolean =
    l.symbols.forall(_.isMarked)
  def mark(call: Int): Line =
    val toMark = Symbol(call, false)
    if l.symbols.contains(toMark) then
      Line(toMark.mark :: l.symbols.filterNot(_ == toMark))
    else l
  def score: Int =
    l.symbols.map(s => if s.isMarked then 0 else s.value).sum
}

case class Board(rows: List[Line], columns: List[Line])
extension(b: Board) {
  def hasWon: Boolean =
    b.rows.exists(_.hasWon) || b.columns.exists(_.hasWon)
  def mark(call: Int): Board =
    Board(b.rows.map(_.mark(call)), b.columns.map(_.mark(call)))
  def score: Int = b.rows.map(_.score).sum
  def draw(): Unit =
    b.rows.map(l => l.symbols.map(x => if x.isMarked then s"*${x.value}*" else x.value.toString)).foreach(println)
}

object Puzzle {
  val input: List[String] = Source
    .fromResource("2021/Day04/Input.txt")
    .getLines
    .toList
  val drawOrder: Seq[Int] = input.head.split(',').map(_.toInt)
  val boards: List[Board] = input
    .tail
    .filterNot(_ == "")
    .sliding(5, 5)
    .toList
    .map(board => board.map(rows => rows.trim.split("\\W+").toList.map(_.toInt)))
    .map{ board =>
      val rows = board.map(line => Line(line.map(Symbol(_, false))))
      val columns = board.transpose.map(line => Line(line.map(Symbol(_, false))))
      Board(rows, columns)
    }


  def part1(): Unit =
    val winner = bingo(boards, drawOrder, 0)
    val winningBoard = winner._1
    val score = winner._1.score * winner._2
    println(s"Part 1: $score")

  def part2(): Unit =
    val loser = loseBingo(boards, drawOrder, 0, boards)
    val losingBoard = loser._1
    val score = loser._1.score * loser._2
    println(s"Part 2: $score")

  @tailrec
  def bingo(boards: List[Board], drawOrder: Seq[Int], lastCall: Int): (Board, Int) =
    if boards.exists(_.hasWon) then
      (boards.filter(_.hasWon).head, lastCall)
    else
      val call = drawOrder.head
      val markedBoards = boards.map(_.mark(call))
      bingo(markedBoards, drawOrder.tail, drawOrder.head)

  def loseBingo(boards: List[Board], drawOrder: Seq[Int], lastCall: Int, lastBoards: List[Board]): (Board, Int) =
    if boards.forall(_.hasWon) then
      (lastBoards.head.mark(lastCall), lastCall)
    else
      val notYetWon = boards.filterNot(_.hasWon)
      val call = drawOrder.head
      val markedBoards = notYetWon.map(_.mark(call))
      loseBingo(markedBoards, drawOrder.tail, drawOrder.head, notYetWon)

}
