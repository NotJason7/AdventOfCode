package com.jbullock.aoc2023.day07

import com.jbullock.aoc2023.day07.HandType._

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2023/Day07/Input.txt").getLines.toVector
  val hands = input.map { case s"$cards $bid" =>
    Hand(cards.map(c => Card.fromOrdinal(Card.cardOrder.indexOf(c))).toVector, bid.toInt)
  }
  val sorted = hands.sortWith((p1, p2) => p2.isStrongerThan(p1))
  val part1  = sorted.zipWithIndex.map((hand, rank) => hand.bid * (rank + 1)).sum
  println(s"Part 1: $part1")
  val jokerSorted = hands.sortWith((p1, p2) => p2.isStrongerThan(p1, true))
  jokerSorted.foreach(println)
  val part2 = jokerSorted.zipWithIndex.map((hand, rank) => hand.bid * (rank + 1)).sum
  println(s"Part 2: $part2")

enum Card:
  case Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two
object Card:
  val cardOrder: String             = "AKQJT98765432"
  def jokerOrdinal(card: Card): Int = if card == Card.Jack then 14 else card.ordinal

enum HandType:
  case FiveKind, FourKind, FullHouse, ThreeKind, TwoPair, OnePair, HighCard

case class Hand(cards: Vector[Card], bid: Int):
  override def toString: String =
    s"$cards @ $bid: hand strength = $handType (${handType.ordinal}) or $jokerlessHandType removing the $jokerCount jokers, with joker  $jokerHandType (${jokerHandType.ordinal})"
  val maxFrequency: Int = cards.distinct.map(card => cards.count(_ == card)).maxOption.getOrElse(0)
  val minFrequency: Int = cards.distinct.map(card => cards.count(_ == card)).minOption.getOrElse(0)
  val pairCount: Int    = cards.distinct.count(card => cards.count(_ == card) == 2)
  val handType: HandType =
    if maxFrequency == 5 then FiveKind
    else if maxFrequency == 4 then FourKind
    else if maxFrequency == 3 && minFrequency == 2 then FullHouse
    else if maxFrequency == 3 then ThreeKind
    else if pairCount == 2 then TwoPair
    else if pairCount == 1 then OnePair
    else HighCard

  val jokerCount: Int             = cards.count(_ == Card.Jack)
  def jokerlessHandType: HandType = Hand(cards.filterNot(_ == Card.Jack), bid).handType
  def jokerHandType: HandType =
    (jokerlessHandType, jokerCount) match
      case (FourKind, 1)  => FiveKind  //AAAA + J
      case (ThreeKind, 2) => FiveKind  //AAA  + JJ
      case (ThreeKind, 1) => FourKind  //AAAB + J
      case (TwoPair, 1)   => FullHouse //AABB + J
      case (OnePair, 3)   => FiveKind  //AA   + JJJ
      case (OnePair, 2)   => FourKind  //AAB  + JJ
      case (OnePair, 1)   => ThreeKind //AABC + J
      case (HighCard, 4)  => FiveKind  //A    + JJJJ
      case (HighCard, 3)  => FourKind  //AB   + JJJ
      case (HighCard, 2)  => ThreeKind //ABC  + JJ
      case (HighCard, 1)  => OnePair   //ABCD + J
      case (FullHouse, x) if x > 0 =>
        println(s"Wtf, $FullHouse with $x jokers")
        FullHouse
      case (_, 5) => FiveKind //       JJJJJ
      case _      => jokerlessHandType

  def isStrongerThan(otherHand: Hand, isJoker: Boolean = false): Boolean =
    val p1Strength = if isJoker then jokerHandType.ordinal else handType.ordinal
    val p2Strength = if isJoker then otherHand.jokerHandType.ordinal else otherHand.handType.ordinal
    if p1Strength < p2Strength then true
    else if p1Strength > p2Strength then false
    else
      cards
        .zip(otherHand.cards)
        .filter((p1, p2) => p1 != p2)
        .map { (p1, p2) =>
          if isJoker then Card.jokerOrdinal(p1) < Card.jokerOrdinal(p2)
          else p1.ordinal < p2.ordinal
        }
        .head
