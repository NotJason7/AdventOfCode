package com.jbullock.aoc2022.day11

@main def solvePuzzle(): Unit =
  val input   = io.Source.fromResource("aoc/2022/Day11/Sample.txt").getLines.toVector.filter(_.nonEmpty)
  val calm    = Round(0, input.grouped(6).toVector.map(v => Monkey.fromInputVector(v, false)))
  val round20 = (1 to 20).foldLeft(calm)((round, _) => round.play)
  val part1   = round20.monkeyBusiness
  println(s"Part 1: $part1")
  val panicked = Round(0, input.grouped(6).toVector.map(v => Monkey.fromInputVector(v, true)))
  println(panicked.monkeys.map(_.inspectTestValue).product)
//  val round10k = (1 to 10000).foldLeft(panicked)((round, _) => round.play)
//  val part2    = round10k.monkeyBusiness
//  println(s"Part 2: $part2")

case class Round(roundNumber: Int, monkeys: Vector[Monkey]):
  def play: Round = Round(
    roundNumber + 1,
    (0 until monkeys.size).foldLeft(monkeys) { (currentMonkeys, turn) =>
      val (emptyHandedMonkey, throws) = currentMonkeys(turn).throwItems
      throws
        .foldLeft(currentMonkeys) { (catchingMonkeys, thrownItems) =>
          thrownItems match
            case (targetMonkey, items) =>
              val updatedMonkey = catchingMonkeys(targetMonkey).catchItems(items)
              catchingMonkeys.updated(targetMonkey, updatedMonkey)
        }
        .updated(turn, emptyHandedMonkey)
    }
  )
  def monkeyBusiness: Int = monkeys.map(_.totalInspections).sortWith(_ > _).take(2).product

case class Item(currentWorkingValue: Int, globalDivisorMultiples: Int, globalDivisor: Int)

case class Monkey(
    id: Int,
    items: Vector[BigInt],
    nextMonkey: BigInt => (Int, BigInt),
    totalInspections: Int,
    inspectTestValue: Int
):
  def throwItems: (Monkey, Vector[(Int, Vector[BigInt])]) =
    val newMonkey   = this.copy(items = Vector.empty[BigInt], totalInspections = totalInspections + items.size)
    val thrownItems = items.map(nextMonkey).groupBy(_._1).view.mapValues(_.map(_._2)).toVector
    (newMonkey, thrownItems)
  def catchItems(thrownItems: Vector[BigInt]): Monkey = this.copy(items = items ++ thrownItems)
object Monkey:
  def fromInputVector(v: Vector[String], isPanicked: Boolean): Monkey =
    val numbers = "[0-9]+".r
    val id      = numbers.findAllIn(v.head).toVector.head.toInt
    val items   = numbers.findAllIn(v(1)).toVector.map(_.toInt).map(BigInt.apply)
    val operation =
      val worryOperation = v(2).drop(23).split(" ").toVector match
        case Vector("*", n) => (i: BigInt) => i * (if n == "old" then i else n.toInt)
        case Vector("+", n) => (i: BigInt) => i + (if n == "old" then i else n.toInt)
        case _              => throw RuntimeException(s"Unable to parse operation ${v(2)}")
      (i: BigInt) => if !isPanicked then worryOperation(i) / 3 else worryOperation(i)
    val testValue   = v(3).drop(21).toInt
    val trueMonkey  = v(4).drop(29).toInt
    val falseMonkey = v(5).drop(30).toInt
    val nextMonkey = (item: BigInt) =>
      val newWorryValue = operation(item)
      (if newWorryValue % testValue == 0 then trueMonkey else falseMonkey, newWorryValue)
    Monkey(id, items, nextMonkey, 0, testValue)
