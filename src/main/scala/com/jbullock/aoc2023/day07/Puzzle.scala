package com.jbullock.aoc2023.day07

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2023/Day07/Input.txt").getLines.toVector
  val hands = input.map { case s"$cards $bid" =>
    Hand(cards.map(c => Card.fromOrdinal(Card.cardOrder.indexOf(c))).toVector, bid.toInt)
  }
  val sorted = hands.sortWith((p1, p2) => p2.isStrongerThan(p1))
  sorted.foreach(println)
  val part1 = sorted.zipWithIndex.map((hand, rank) => hand.bid * (rank + 1)).sum
  println(s"Part 1: $part1")

enum Card:
  case Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two
object Card:
  val cardOrder: String = "AKQJT98765432"

extension (cards: Vector[Card])
  def maxFrequency: Int = cards.distinct.map(card => cards.count(_ == card)).max
  def minFrequency: Int = cards.distinct.map(card => cards.count(_ == card)).min

case class Hand(cards: Vector[Card], bid: Int):
  override def toString: String = s"$cards @ $bid: hand strength = $handStrength"
  val isFiveOfAKind: Boolean    = cards.maxFrequency == 5
  val isFourOfAKind: Boolean    = cards.maxFrequency == 4
  val isFullHouse: Boolean      = cards.maxFrequency == 3 && cards.minFrequency == 2
  val isThreeOfAKind: Boolean   = cards.maxFrequency == 3 && cards.minFrequency == 1
  val isTwoPair: Boolean        = cards.distinct.count(card => cards.count(_ == card) == 2) == 2
  val isOnePair: Boolean        = cards.distinct.length == 4
  val isHighCard: Boolean       = cards.distinct.length == 5
  val handStrength: Int =
    if isFiveOfAKind then 7
    else if isFourOfAKind then 6
    else if isFullHouse then 5
    else if isThreeOfAKind then 4
    else if isTwoPair then 3
    else if isOnePair then 2
    else 1

  def isStrongerThan(otherHand: Hand): Boolean =
    if handStrength > otherHand.handStrength then true
    else if handStrength < otherHand.handStrength then false
    else cards.zip(otherHand.cards).filter((p1, p2) => p1 != p2).map { (p1, p2) => p1.ordinal < p2.ordinal }.head
