package com.jbullock.aoc2022.day09.day10

@main def solvePuzzle(): Unit =
  val input     = io.Source.fromResource("aoc/2022/Day10/Input.txt").getLines.toVector
  val signals   = input.map(_.drop(5).toIntOption)
  val processed = CPU(Vector(1)).runSignals(signals)
  val part1     = (20 to 220 by 40).toVector.flatMap(processed.signalStrengthAt).sum
  println(s"Part 1: $part1")
  val pixels = (0 until processed.registerHistory.length).toVector
    .map(crt => if processed.registerHistory(crt).window(1).contains(crt % 40) then "███" else "   ")
    .grouped(40)
    .toVector
    .map(_.mkString)
  pixels.prepended("Part 2:").foreach(println)

extension (i: Int) def window(n: Int): Set[Int] = (i - n to i + n).toSet

case class CPU(registerHistory: Vector[Int]):
  def signalStrengthAt(n: Int): Option[Int] = registerHistory.lift(n - 1).map(_ * n)
  def addRegister(delta: Int): CPU          = this.copy(registerHistory :+ (registerHistory.last + delta))
  def runSignal(signal: Option[Int]): CPU = signal match
    case Some(delta) => addRegister(0).addRegister(delta)
    case None        => addRegister(0)
  def runSignals(signals: Vector[Option[Int]]): CPU = signals.foldLeft(this)((cpu, signal) => cpu.runSignal(signal))
