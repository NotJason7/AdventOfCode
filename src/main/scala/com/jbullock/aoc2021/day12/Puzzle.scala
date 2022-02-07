package com.jbullock.aoc2021.day12

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  Puzzle.part1()
  Puzzle.part2()

case class Node(name: String)

extension (n: Node) {
  def isBig: Boolean =
    n.name == n.name.capitalize
  def isStart: Boolean =
    n.name == "start"
  def isEnd: Boolean =
    n.name == "end"
}

case class Route(path: List[Node])

extension (r: Route) {
  def extend(n: Node): Route =
    Route(n :: r.path)
  def duplicateCount: Int =
    r.path.filterNot(_.isBig).groupBy(identity).map((k, v) => v.size).count(_ >= 2)
  def nodeCount(n: Node): Int =
    r.path.count(_ == n)
  def canExtendSmall(n: Node): Boolean =
    val extended = r.extend(n)
    extended.duplicateCount <= 1 && extended.nodeCount(n) <= 2
}

object Puzzle:
  val input: List[String] = Source
    .fromResource("2021/Day12/Input.txt")
    .getLines
    .toList
  val cave: Map[Node, List[Node]] = input
    .map(_.split('-').toList.map(Node.apply))
    .flatMap(list => List((list.head, list(1)), (list(1),list.head)))
    .groupBy(_(0))
    .map((k, v) => k -> v.map(_._2))


  def part1(): Unit =
    val duplicateRule = (n: Node, r: Route) => !r.path.contains(n) || n.isBig
    val answer = findRoutes(cave, duplicateRule).length
    println(s"Part 1: $answer")

  def part2(): Unit =
    val duplicateRule = (n: Node, r: Route) => !n.isStart && r.canExtendSmall(n) || n.isBig
    val answer = findRoutes(cave, duplicateRule).length
    println(s"Part 2: $answer")


  def findRoutes(cave: Map[Node, List[Node]], duplicateRule: (Node, Route) => Boolean): List[Route] =
    val start = Node("start")
    val end = Node("end")
    val startingRoutes = List(Route(List(start)))

    @tailrec
    def loop(routes: List[Route]): List[Route] =
      if routes.forall(_.path.head.isEnd) then routes
      else
        val finishedRoutes = routes.filter(_.path.head.isEnd)
        val unfinishedRoutes = routes.filterNot(_.path.head.isEnd)
        val updatedRoutes = unfinishedRoutes.flatMap { r =>
          val extensions = cave(r.path.head).filter(n => duplicateRule(n, r))
          extensions.map(n => r.extend(n))
        }
        loop(finishedRoutes ::: updatedRoutes)

    loop(startingRoutes)
