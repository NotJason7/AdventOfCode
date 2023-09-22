package com.jbullock.aoc2020.day17

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2020/Day17/Input.txt").getLines.toVector
  val initiallyActive = (for
    y <- input.indices
    x <- input.head.indices
    if input(y)(x) == '#'
  yield Position(x, y, 0, 0)).toSet
  val part1 = State(initiallyActive).nextN(6, 3).cubes.size
  println(s"Part 1: $part1")
  val part2 = State(initiallyActive).nextN(6, 4).cubes.size
  println(s"Part 2: $part2")

case class State(cubes: Set[Position]):
  def next(dimensions: Int): State =
    val newActives  = cubes.flatMap(_.adjacent(dimensions)).filter(_.adjacent(dimensions).count(cubes.contains) == 3)
    val stillActive = cubes.filter(cube => Set(2, 3).contains(cube.adjacent(dimensions).count(cubes.contains)))
    State(newActives ++ stillActive)
  @tailrec final def nextN(n: Int, dimensions: Int): State =
    if n == 0 then this else next(dimensions).nextN(n - 1, dimensions)

case class Position(x: Int, y: Int, z: Int, t: Int):
  def adjacent(dimensions: Int): Set[Position] =
    val allAdjacent = (for
      dx <- (-1 to 1)
      dy <- (-1 to 1)
      dz <- (-1 to 1)
      dt <- (-1 to 1)
      if dx != 0 || dy != 0 || dz != 0 || dt != 0
    yield Position(x + dx, y + dy, z + dz, t + dt)).toSet
    if dimensions == 3 then allAdjacent.filter(_.t == t) else allAdjacent
