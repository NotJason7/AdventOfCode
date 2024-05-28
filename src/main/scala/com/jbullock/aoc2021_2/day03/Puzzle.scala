package com.jbullock.aoc2021_2.day03

@main def solvePuzzle(): Unit =
  val input: Seq[Seq[Int]] =
    scala.io.Source.fromResource("aoc/2021/Day03/Input.txt").getLines.toSeq.map(_.map(_.asDigit))
  val indices  = input.head.indices
  val gamma    = indices.map(input.mostCommonNth).binaryStringToInt
  val epsilon = indices.map(input.leastCommonNth).binaryStringToInt
  val part1    = gamma * epsilon
  println(s"Part 1: $part1")
  val oxygenGeneratorRating: Int = indices
    .foldLeft(input)((current, index) => current.filterMostCommonNth(index))
    .head
    .binaryStringToInt
  val CO2ScrubberRating: Int = indices
    .foldLeft(input)((current, index) => current.filterLeastCommonNth(index))
    .head
    .binaryStringToInt
  val part2 = oxygenGeneratorRating * CO2ScrubberRating
  println(s"Part 2: $part2")

extension (is: Seq[Int]) def binaryStringToInt: Int = Integer.parseInt(is.mkString, 2)

extension (is: Seq[Seq[Int]])
  def countNthElements(n: Int): Seq[(Int, Int)] = is.map(_(n)).groupBy(identity).map((i, es) => (i, es.size)).toSeq
  def mostCommonNth(n: Int): Int                = countNthElements(n).maxBy(_._2)._1
  def leastCommonNth(n: Int): Int               = countNthElements(n).minBy(_._2)._1
  def filterMostCommonNth(n: Int): Seq[Seq[Int]] =
    if is.length == 1 then is
    else
      val mostCommonN = if is.mostCommonNth(n) == is.leastCommonNth(n) then 1 else is.mostCommonNth(n)
      is.filter(seq => seq(n) == mostCommonN)
  def filterLeastCommonNth(n: Int): Seq[Seq[Int]] =
    if is.length == 1 then is
    else
      val leastCommonN = if is.mostCommonNth(n) == is.leastCommonNth(n) then 0 else is.leastCommonNth(n)
      is.filter(seq => seq(n) == leastCommonN)
