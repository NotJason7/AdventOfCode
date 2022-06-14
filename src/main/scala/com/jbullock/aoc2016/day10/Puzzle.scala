package com.jbullock.aoc2016.day10

import scala.annotation.tailrec
import scala.io.Source
//
//@main
//def solvePuzzle(): Unit =
//  val input: Vector[String] = Source.fromResource("aoc/2016/Day10/Example.txt").getLines.toVector
//  println(s"Part1: ${Puzzle.part1(input)}")
////  println(s"Part2: ${Puzzle.part2(input)}")
//
//object Puzzle:
//  def part1(input: Vector[String]): String =
//    val emptyState = State(Map.empty[Bot, Set[Chip]], Map.empty[Output, Set[Chip]], Map.empty[Bot, Rule])
//    val initialState = emptyState.parseCommands(input)
//    println(initialState)
////    val nextState = initialState.tick
//    val test = Map(2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
//    val toRemove = List(3, 4)
//    val removed = toRemove.foldLeft(test)(_ - _)
//    println(removed)
////    println(input.filter(_.startsWith("bot")).map(_.replace(" gives low to bot ",",").replace(" and high to bot ",",")))
//    "lol"
//
//
//case class Chip(value: Int)
//trait ChipHolder
//case class Bot(id: Int) extends ChipHolder
//case class Output(id: Int) extends ChipHolder
//case class Rule(low: ChipHolder, high: ChipHolder)
//case class State(bots: Map[Bot, Set[Chip]], outputs: Map[Output, Set[Chip]], rules: Map[Bot, Rule]):
//  def parseCommand(command: String): State =
//    command.split("\\D+").filter(_.nonEmpty).toVector.map(_.toInt) match
//      case Vector(id1, id2) =>
//        val bot = Bot(id2)
//        val botChipSet = (bot, bots.getOrElse(bot, Set.empty[Chip]) + Chip(id1))
//        State(bots + botChipSet, outputs, rules)
//      case Vector(id1, idLow, idHigh) =>
//        val context = command.split("(?<=\\d)").toVector
//        val receiveLow = if context(1).contains("bot") then Bot(idLow) else Output(idLow)
//        val receiveHigh = if context(2).contains("bot") then Bot(idHigh) else Output(idHigh)
//        State(bots, outputs, rules + (Bot(id1) -> Rule(receiveLow, receiveHigh)))
//      case _ =>
//        throw new RuntimeException(s"Unhandled command: $command")
//
//  def parseCommands(commands: Vector[String]): State =
//    if commands.isEmpty then this else
//      this.parseCommand(commands.head).parseCommands(commands.tail)
//
//  def removeChips(givingBots: Set[Bot]): Map[Bot, Set[Chip]] =
//    bots.filter(x => givingBots.contains(x._1)).keys.foldLeft(bots)(_ - _)
//
//  def giveChip(toReceive: Map[Bot, Chip]): Map[Bot, Chip] =
//    if bots.contains(toReceive._1) then
//      ()
//  def tick: State =
//    val botsToLoseChips = bots.filter(_._2.size == 2)
//    val botsToGainChips = botsToLoseChips.flatMap { (giver: Bot, chips: Set[Chip]) =>
//      val receivers = rules(giver)
//      val high = chips.maxBy(_.value)
//      val low = chips.minBy(_.value)
//      Map(receivers.low -> low, receivers.high -> high)
//    }
//    removeChips(botsToLoseChips)+ botsToGainChips
//    println(botsToGainChips)
//    this


//case class Chip(value: Int)
//trait ChipHolder
//case class Output(id: Int, chips: Set[Chip]) extends ChipHolder
//case class Bot(id: Int, chips: Set[Chip], lowGive: Option[ChipHolder], highGive: Option[ChipHolder]) extends ChipHolder
//case class State(bots: Seq[Bot], outputs: Seq[Output]):
//  def parseLine(line: String): State =

//  def addRule(rule: (Value, Bot | Output)): Bot = this.copy(rules = rules + rule)
//  def followRule:
//  def receiveChip(c: Chip): Bot =
//    val updatedChips = c::chips
//    if updatedChips.length == 2 then
//      followRule
//
//enum Value:
//  case Low, High
//
//case class BotOrders(lowMap: Map[Int, Int], highMap: Map[Int, Int])
