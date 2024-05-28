package com.jbullock.aoc2021_2.day04

@main def solvePuzzle(): Unit =
  val input     = scala.io.Source.fromResource("aoc/2021/Day04/Example.txt").getLines.toSeq
  val drawOrder = input.head
  val cards =
    input
      .drop(2)
      .filterNot(_.isBlank)
      .map(s => s.trimSpaces.split(' ').toSeq.map(_.toInt))
      .grouped(5)
      .map(rows => Card(rows))
  cards.foreach(_.show())

extension (s: String)
  def trimSpaces: String = s.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse.replaceAll(" +", " ")

extension(rows: Seq[Seq[Int]]) def hasWinningRow(marked: Set[Int]): Boolean = rows.exists(_.forall(marked.contains))

case class Card(rows: Seq[Seq[Int]], marked: Set[Int] = Set.empty[Int]):
  private val columns: Seq[Seq[Int]] = rows.transpose
  def mark(i: Int): Card = this.copy(marked = marked + i)
  def hasWon: Boolean = rows.hasWinningRow(marked) || columns.hasWinningRow(marked)
  def show(): Unit = rows.foreach(row => println(row.mkString(",")))
