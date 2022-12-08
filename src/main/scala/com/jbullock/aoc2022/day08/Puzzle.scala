package com.jbullock.aoc2022.day08

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = io.Source.fromResource("aoc/2022/Day08/Input.txt").getLines.toVector
  val trees = for
    y <- input.indices
    x <- input.head.indices
  yield Tree(Position(x, y), input(y)(x).asDigit)
  val leftToRight  = trees.grouped(input.head.length).toVector
  val left         = leftToRight.flatMap(_.filterVisible)
  val right        = leftToRight.map(_.reverse).flatMap(_.filterVisible)
  val top          = leftToRight.transpose.flatMap(_.filterVisible)
  val bottom       = leftToRight.transpose.map(_.reverse).flatMap(_.filterVisible)
  val visibleTrees = (left ++ right ++ top ++ bottom).distinct.size
  println(s"Part 1: $visibleTrees")
  given Map[Position, Int] = trees.flatMap(_.asMap).toMap
  val bestScore            = trees.map(_.scenicScore).max
  println(s"Part 2: $bestScore")

sealed trait Direction(val x: Int, val y: Int)
case object Up    extends Direction(0, -1)
case object Down  extends Direction(0, 1)
case object Left  extends Direction(-1, 0)
case object Right extends Direction(1, 0)

case class Position(x: Int, y: Int):
  def move(direction: Direction)(using trees: Map[Position, Int]): Position = Position(x + direction.x, y + direction.y)

case class Tree(position: Position, height: Int):
  def asMap: Map[Position, Int]                         = Map(position -> height)
  def scenicScore(using trees: Map[Position, Int]): Int = Vector(Up, Down, Left, Right).map(lineScore).product
  def lineScore(direction: Direction)(using trees: Map[Position, Int]): Int =
    @tailrec def loop(current: Position, visible: Int): Int =
      trees.get(current) match
        case Some(tree) if tree >= height => visible + 1
        case Some(_)                      => loop(current.move(direction), visible + 1)
        case _                            => visible
    loop(position.move(direction), 0)

extension (s: IndexedSeq[Tree])
  def filterVisible: Vector[Tree] = s
    .foldLeft((-1, Vector.empty[Tree])) { (accumulator, tree) =>
      accumulator match
        case (highestSeen: Int, visibleTrees: Vector[Tree]) =>
          if tree.height > highestSeen then (tree.height, visibleTrees.appended(tree))
          else (highestSeen, visibleTrees)
    }
    ._2
