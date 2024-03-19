package com.jbullock.aoc2023.day04

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2023/Day04/Input.txt").getLines.toVector
  val cards = input.flatMap(Card.fromString)
  val part1 = cards.map(_.score).sum
  println(s"Part 1: $part1")
  val part2 = findScore(cards)
  println(s"Part 2: $part2")

def findScore(cards: Vector[Card]): Int =

  @tailrec def loop(cardsLeft: Vector[Card], cardQuantities: Map[Int, Int], cardsWon: Map[Card, Int]): Int =
    cardsLeft.headOption match
      case Some(currentCard) =>
        val currentIndex = cards.indexOf(currentCard)
        val quantity     = cardQuantities.getOrElse(currentIndex, 1)
        val nextCardsWon = cardsWon.updated(currentCard, quantity)
        val cardsWonByCurrentCard = (currentIndex to currentIndex + currentCard.winningCardNumbers)
          .map(index => index -> (quantity + cardQuantities.getOrElse(index, 0)))
          .toMap
        val nextCardQuantities = cardQuantities ++ cardsWonByCurrentCard
        loop(cardsLeft.tail, nextCardQuantities, nextCardsWon)
      case None => cardsWon.values.sum

  val initialCardQuantities = cards.indices.map(_ -> 1).toMap
  val initialCardsWon       = cards.map(_ -> 1).toMap
  loop(cards, initialCardQuantities, initialCardsWon)

case class Card(winningNumbers: Vector[Int], cardNumbers: Vector[Int]):
  val winningCardNumbers: Int = cardNumbers.count(winningNumbers.contains)
  val score: Int              = math.pow(2, winningCardNumbers - 1).floor.toInt
object Card:
  def fromString(s: String): Option[Card] = s.replace("  ", " ") match
    case s"$_: $winningString | $cardString" =>
      val winningNumbers = winningString.split(' ').map(_.toInt).toVector
      val cardNumbers    = cardString.split(' ').map(_.toInt).toVector
      Some(Card(winningNumbers, cardNumbers))
