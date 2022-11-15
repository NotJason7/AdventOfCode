package com.jbullock.aoc2017.day24

import scala.annotation.tailrec
import scala.io.Source

@main def solvePuzzle(): Unit =
  val input                  = Source.fromResource("aoc/2017/Day24/Input.txt").getLines.toVector
  given Vector[Block]        = input.map(_.split('/').toVector).map { case Vector(k, v) => Block(k.toInt, v.toInt) }
  val possibleBridges        = buildBridges(0)
  val strongestBridge        = possibleBridges.maxBy(_.strength)
  val longestBridgeLength    = possibleBridges.maxBy(_.blocks.size).blocks.size
  val longestStrongestBridge = possibleBridges.filter(_.blocks.size == longestBridgeLength).maxBy(_.strength)
  println(s"Part 1: ${strongestBridge.strength}")
  println(s"Part 2: ${longestStrongestBridge.strength}")

case class Block(a: Int, b: Int):
  def strength: Int             = a + b
  def contains(i: Int): Boolean = a == i || b == i
  def reverse: Block            = Block(b, a)
  override def toString         = s"$a/$b"

case class Bridge(blocks: Vector[Block])(using allBlocks: Vector[Block]):
  def strength: Int = blocks.map(_.strength).sum
  def end: Int      = blocks.last.b
  def extensions: Vector[Block] = allBlocks
    .filterNot(block => blocks.contains(block) || blocks.map(_.reverse).contains(block))
    .filter(_.contains(end))
  def isTerminal: Boolean = extensions.isEmpty
  def append(block: Block): Bridge =
    if block.a == end then Bridge(blocks.appended(block))
    else if block.b == end then Bridge(blocks.appended(block.reverse))
    else throw new RuntimeException(s"Tried to append block $block to bridge $this")
  def extend: Vector[Bridge] =
    if isTerminal then Vector(this)
    else extensions.map(append)
  override def toString: String = blocks.map(_.toString).mkString("--")

def buildBridges(start: Int)(using allBlocks: Vector[Block]): Set[Bridge] =

  @tailrec def loop(terminals: Set[Bridge], bridges: Set[Bridge]): Set[Bridge] =
    if bridges.isEmpty then terminals
    else
      val extensions    = bridges.flatMap(_.extend)
      val nextTerminals = terminals ++ extensions.filter(_.isTerminal)
      val nextBridges   = extensions.filterNot(_.isTerminal)
      loop(nextTerminals, nextBridges)

  val startBridge = Bridge(Vector(Block(start, start)))
  loop(Set.empty[Bridge], Set(startBridge))
