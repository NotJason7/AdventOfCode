package com.jbullock.aoc2017.day07

import io.Source

@main
def solvePuzzle(): Unit =
  println(s"Part 1: ${Puzzle.part1}")
//  println(s"Part 2: ${Puzzle.part2}")

object Puzzle:
  val input: Vector[String] = Source.fromResource("aoc/2017/Day07/Input.txt").getLines.toVector
  val bases = input.filter(_.contains("->")).map(_.takeWhile(_ != ' '))
  val balanced = input.filter(_.contains("->")).flatMap(_.dropWhile(_ != '>').drop(2).split(", ").toVector)
  val start = bases.filterNot(balanced.contains).head
  def part1: String = start

//  def part2: Int =
//    val tree = buildTree(start, input)
//    println(tree)
//    1


//case class Node(name: String, weight: Int)
//object Node:
//  def fromString(s: String): Node =
//    s.replaceAll("(|)","").split(" ").toVector match
//      case (name: String, weight: String) => Node(name, weight.toInt)
//
//sealed trait Tree:
//  def weight: Int
//case class Leaf(node: Node) extends Tree:
//  def weight: Int = node.weight
//case class Branch(node: Node, branches: Vector[Tree]) extends Tree:
//  def weight: Int = node.weight + branches.map(_.weight).sum

//def buildTree(start: String, treeText: Vector[String]): Tree =
//
//  def loop(name: String): Tree =
//    val current = treeText.filter(_.startsWith(s"$name ")).head
//    current.split(" -> ").toVector match
//      case Vector(base: String, balanced: String) => Branch(Node.fromString(base), balanced.map(loop))
//      case (base: String, Nil) => Leaf(Node.fromString(base))
//      case e => throw new RuntimeException(s"Unable to parse: $e")
//
//  loop(start)
