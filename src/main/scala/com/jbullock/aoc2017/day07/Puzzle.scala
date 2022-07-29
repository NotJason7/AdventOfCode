package com.jbullock.aoc2017.day07

import io.Source
import scala.annotation.tailrec

@main
def solvePuzzle(): Unit =
  println(s"Part 1: ${Puzzle.part1}")
  println(s"Part 2: ${Puzzle.part2}")

object Puzzle:
  val input: Vector[String] = Source.fromResource("aoc/2017/Day07/Input.txt").getLines.toVector
  val bases = input.filter(_.contains("->")).map(_.takeWhile(_ != ' '))
  val balanced = input.filter(_.contains("->")).flatMap(_.dropWhile(_ != '>').drop(2).split(", ").toVector)
  val start = bases.filterNot(balanced.contains).head
  def part1: String = start

  def part2: Int =
    val tree = buildTree(start, input)
    findImbalanced(tree, 0)

@tailrec
def findImbalanced(tree: Tree, weight: Int): Int =
  val balance = tree.branch.map(t => (t.n.weight, t.weight)).groupBy(_._2).toVector
  if tree.isBalanced then tree.n.weight + weight
  else
    val common = balance.maxBy(_._2.size)._1
    val uncommon = balance.minBy(_._2.size)._1
    val fix = common - uncommon
    val next = tree.branch.filter(_.weight == uncommon).head
    findImbalanced(next, fix)


case class Node(name: String, weight: Int)
object Node:
  def fromString(s: String): Node =
    val Vector(name, weight) =
      s.replace("(","").replace(")","").split(" ").toVector
    Node(name, weight.toInt)

sealed trait Tree(node: Node):
  def n: Node
  def weight: Int
  def branch: Vector[Tree]
  def isBalanced: Boolean
case class Leaf(node: Node) extends Tree(node: Node):
  def n: Node = node
  def weight: Int = node.weight
  def branch: Vector[Tree] = Vector.empty[Tree]
  def isBalanced: Boolean = true
case class Branch(node: Node, branches: Vector[Tree]) extends Tree(node: Node):
  def n: Node = node
  def weight: Int = node.weight + branches.map(_.weight).sum
  def branch: Vector[Tree] = branches
  def isBalanced: Boolean = branches.map(_.weight).toSet.size == 1

def buildTree(start: String, treeText: Vector[String]): Tree =

  def loop(name: String): Tree =
    val current = treeText.filter(_.startsWith(s"$name ")).head
    if current.contains(" -> ") then
      current.split(" -> ").toVector match
      case Vector(base: String, balanced: String) =>
        val branches = balanced.split(", ").toVector
        Branch(Node.fromString(base), branches.map(loop))
    else Leaf(Node.fromString(current))

  loop(start)
