package com.jbullock.aoc2022.day11

@main def solvePuzzle(): Unit =
  val input = io.Source.fromResource("aoc/2022/Day11/Input.txt").getLines.toVector.filter(_.nonEmpty)
  given Int = input.filter(_.startsWith("  Test")).map(_.drop(21).toInt).product
  val calm  = Round(input.grouped(6).toVector.map(v => Monkey.fromInputVector(v, false)))
  val part1 = (1 to 20).foldLeft(calm)((round, _) => round.play).monkeyBusiness
  println(s"Part 1: $part1")
  val panicked = Round(input.grouped(6).toVector.map(v => Monkey.fromInputVector(v, true)))
  val part2    = (1 to 10000).foldLeft(panicked)((round, _) => round.play).monkeyBusiness
  println(s"Part 2: $part2")

type Item = BigInt

case class Round(monkeys: Vector[Monkey]):
  def monkeyBusiness: BigInt = monkeys.map(_.inspections).sortWith(_ > _).take(2).product
  def play: Round = Round(monkeys.indices.foldLeft(monkeys) { (roundMonkeys, activeMonkey) =>
    val (activeMonkeyNoItems, thrownItems) = roundMonkeys(activeMonkey).throwItems
    val monkeysReadyToCatch                = roundMonkeys.updated(activeMonkey, activeMonkeyNoItems)
    thrownItems.foldLeft(monkeysReadyToCatch)((catchingMonkeys, item) => item.reachTarget(catchingMonkeys))
  })

case class Thrown(target: Int, item: Item):
  def reachTarget(monkeys: Vector[Monkey]): Vector[Monkey] = monkeys.updated(target, monkeys(target).catchItem(item))

case class Monkey(items: Vector[Item], throwItem: Item => Thrown, inspections: BigInt):
  def throwItems: (Monkey, Vector[Thrown]) =
    (Monkey(Vector.empty[Item], throwItem, inspections + items.size), items.map(throwItem))
  def catchItem(thrownItem: Item): Monkey = this.copy(items = items :+ thrownItem)
object Monkey:
  def fromInputVector(v: Vector[String], youArePanicked: Boolean)(using gcd: Int): Monkey =
    val items                           = "[0-9]+".r.findAllIn(v(1)).toVector.map(_.toInt).map(BigInt.apply)
    val (test, trueTarget, falseTarget) = (v(3).drop(21).toInt, v(4).drop(29).toInt, v(5).drop(30).toInt)
    val inspect =
      val operation = v(2).drop(23).split(" ").toVector match
        case Vector(s, n) if Set("*", "+").contains(s) =>
          (i: Item) =>
            val j: Item = if n == "old" then i else n.toInt
            if s == "*" then i * j else i + j
        case _ => throw RuntimeException(s"Unable to parse operation ${v(2)}")
      (i: Item) => if youArePanicked then operation(i) else operation(i) / 3
    val throwItem = (i: Item) =>
      val worry = inspect(i) % gcd
      Thrown(if worry % test == 0 then trueTarget else falseTarget, worry)
    Monkey(items, throwItem, 0)
