package com.jbullock.aoc2024.day11

@main def solvePuzzle(): Unit =
  val input  = scala.io.Source.fromResource("aoc/2024/Day11/Input.txt").mkString
  val stones = input.split(' ').map(_.toInt).toSeq.map(i => Stone.apply(BigInt(i)))
  val part1  = (1 to 25).foldLeft(stones)((oldStones, _) => oldStones.flatMap(_.blink)).size
  println(s"Part 1: $part1")
  val stoneMap = stones.groupBy(identity).view.map((stone, stones) => (stone, BigInt(stones.size))).toMap
  val part2 = (1 to 75)
    .foldLeft(stoneMap)((oldMap, _) =>
      val oldMapValues = oldMap.view.toSeq
      val newMapValues = oldMapValues.flatMap((stone, count) => stone.blink.map(s => (s, count)))
      val newMap = newMapValues
        .groupBy(_._1)
        .view
        .map { (stone, stoneCounts) =>
          (stone, stoneCounts.map(_._2).sum)
        }
        .toMap
      newMap
    )
    .values
    .sum
  println(s"Part 2: $part2")

case class Stone(number: BigInt):
  def blink: Seq[Stone] = number match
    case 0 => Seq(Stone(BigInt(1)))
    case x if x.toString.length % 2 == 0 =>
      val xLength = x.toString.length
      val a       = Stone(BigInt(x.toString.take(xLength / 2)))
      val b       = Stone(BigInt(x.toString.drop(xLength / 2)))
      Seq(a, b)
    case _ => Seq(Stone(number * BigInt(2024)))
