package com.jbullock.aoc2021_2.day04

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input     = scala.io.Source.fromResource("aoc/2021/Day04/Input.txt").getLines.toSeq
  val drawOrder = input.head.split(',').map(_.toInt).toSeq
  val cards =
    input
      .drop(2)
      .filterNot(_.isBlank)
      .map(s => s.trimSpaces.split(' ').toSeq.map(_.toInt))
      .grouped(5)
      .map(rows => Card(rows))
      .toSeq
  val cardsAtWinningDraw = findScoresAtWinningDraw(cards, drawOrder)
  val part1              = cardsAtWinningDraw.head.score
  println(s"Part 1: $part1")
  val part2 = cardsAtWinningDraw.last.score
  println(s"Part 2: $part2")

def findScoresAtWinningDraw(cards: Seq[Card], draws: Seq[Int]): Seq[Card] =

  @tailrec def loop(currentCards: Seq[Card], drawsLeft: Seq[Int], wonCards: Seq[Card] = Seq.empty[Card]): Seq[Card] =
    val (newlyWonCards, notYetWonCards) = currentCards.partition(_.hasWon)
    val nextWonCards                    = wonCards ++ newlyWonCards
    if notYetWonCards.isEmpty then nextWonCards
    else
      val nextCards = notYetWonCards.map(_.mark(drawsLeft.head))
      loop(nextCards, drawsLeft.tail, nextWonCards)

  loop(cards, draws)

extension (s: String)
  def trimSpaces: String = s.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse.replaceAll(" +", " ")

extension (rows: Seq[Seq[Int]]) def hasWinningRow(marked: Seq[Int]): Boolean = rows.exists(_.forall(marked.contains))

case class Card(rows: Seq[Seq[Int]], marked: Seq[Int] = Seq.empty[Int]):
  private val columns: Seq[Seq[Int]] = rows.transpose
  def mark(i: Int): Card             = this.copy(marked = marked :+ i)
  def hasWon: Boolean                = rows.hasWinningRow(marked) || columns.hasWinningRow(marked)
  def score: Int                     = rows.flatten.filterNot(marked.contains).sum * marked.last
