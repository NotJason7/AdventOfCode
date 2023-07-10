package com.jbullock.aoc2020.day07

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input                   = scala.io.Source.fromResource("aoc/2020/Day07/Input.txt").getLines.toVector
  given bagRules: Vector[Bag] = input.flatMap(Bag.fromString)
  println(s"Part 1: ${countBagsContaining("shiny gold")}")
  println(s"Part 2: ${countBagsContainedBy("shiny gold")}")

def countBagsContaining(target: BagType)(using bags: Vector[Bag]): Int =
  @tailrec def loop(targetBags: Set[BagType], containingBags: Set[BagType]): Int =
    if targetBags.isEmpty then containingBags.size
    else
      val bagsContainingTargetBags: Set[BagType] =
        targetBags.flatMap(bag => bags.filter(_.contents.map(_.bagType).contains(bag)).map(_.bagType))
      val newTargetBags     = bagsContainingTargetBags.diff(containingBags)
      val newContainingBags = containingBags ++ newTargetBags
      loop(bagsContainingTargetBags.diff(containingBags), newContainingBags)
  loop(Set(target), Set.empty[BagType])

def countBagsContainedBy(target: BagType)(using bags: Vector[Bag]): Int =
  val start = CumulativeBag(findBag(target), 1).nextLevel
  @tailrec def loop(bagsToCount: Vector[CumulativeBag], total: Int): Int =
    if bagsToCount.isEmpty then total
    else
      val bagCountAtThisLevel = bagsToCount.map(bag => bag.multiplier).sum
      val newBagsToCount      = bagsToCount.flatMap(_.nextLevel)
      loop(newBagsToCount, total + bagCountAtThisLevel)
  loop(start, 0)

def findBag(target: BagType)(using bags: Vector[Bag]): Bag = bags.filter(_.bagType == target).head

type BagType = String
case class CumulativeBag(bag: Bag, multiplier: Int):
  def nextLevel(using bags: Vector[Bag]): Vector[CumulativeBag] =
    bag.contents.toVector.map(content =>
      val nextLevelBag        = findBag(content.bagType)
      val nextLevelMultiplier = multiplier * content.quantity
      CumulativeBag(nextLevelBag, nextLevelMultiplier)
    )
case class BagContent(bagType: BagType, quantity: Int)
case class Bag(bagType: BagType, contents: Set[BagContent])
object Bag:
  def fromString(s: String): Option[Bag] = s match
    case s"$bagType bags contain no other bags." => Some(Bag(bagType, Set.empty[BagContent]))
    case s"$bagType bags contain $containString" =>
      val canContain = containString
        .split(", ")
        .toVector
        .map { case s"$quantity $contentType bag$_" => BagContent(contentType, quantity.toInt) }
        .toSet
      Some(Bag(bagType, canContain))
    case _ => None
