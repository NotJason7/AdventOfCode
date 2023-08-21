package com.jbullock.aoc2020.day13

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input        = scala.io.Source.fromResource("aoc/2020/Day13/Input.txt").getLines.toVector
  val earliestTime = input.head.toInt
  val busses = input.tail.head
    .split(",")
    .zipWithIndex
    .filterNot(_._1 == "x")
    .map { case (id, index) => Bus(id.toInt, index) }
    .toVector
  val part1 = busses.minBy(_.departureAfter(earliestTime)).waitTimesId(earliestTime)
  println(s"Part 1: $part1")
  val maxBus = busses.maxBy(_.id)
  val start  = SyncedBusses(maxBus.id - maxBus.departAt, maxBus.id, Set(maxBus), busses.toSet - maxBus)
  val part2  = start.syncBusTime
  println(s"Part 2: $part2")

case class SyncedBusses(time: Long, delta: Long, synced: Set[Bus], unsynced: Set[Bus]):
  def next: SyncedBusses =
    val nextSynced = unsynced.filter(bus => bus.isContestValidAt(time))
    val nextDelta  = delta * nextSynced.map(_.id).product
    SyncedBusses(time + nextDelta, nextDelta, synced ++ nextSynced, unsynced -- nextSynced)
  @tailrec final def syncBusTime: Long = if unsynced.isEmpty then time - delta else next.syncBusTime

case class Bus(id: Long, departAt: Long):
  def waitTimesId(minute: Long): Long         = (departureAfter(minute) - minute) * id
  def departureAfter(minute: Long): Long      = (id - (minute       % id)) + minute
  def isContestValidAt(minute: Long): Boolean = (minute + departAt) % id == 0
