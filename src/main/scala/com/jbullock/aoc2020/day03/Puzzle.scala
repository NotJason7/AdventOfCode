package com.jbullock.aoc2020.day03

@main def solvePuzzle(): Unit =
  val input           = scala.io.Source.fromResource("aoc/2020/Day03/Input.txt").getLines.toVector
  val treeMap = for
    y <- input.indices
    x <- 0 until mapWidth
    tile = input(y)(x)
  yield Position(x, y) -> tile
  val part1Trajectory = Trajectory(3, 1)
  val part1 = input
    .drop(1)
    .foldLeft(State(Position.origin, 0)) { case (state: State, mapString: String) =>
      val newPosition = state.position.followTrajectory(part1Trajectory)
      val currentTile = mapString(newPosition.x % mapString.length)
      val hitTree     = if currentTile == '#' then 1 else 0
//      println(
//        s"Character ${newPosition.x} (${newPosition.x % mapString.length}) of $mapString: $currentTile, so $hitTree"
//      )
      State(newPosition, state.treesHit + hitTree)
    }
    .treesHit
  println(s"Part 1: $part1")
  val part2Trajectories = List(
    Trajectory(1, 1), Trajectory(3, 1), Trajectory(5, 1), Trajectory(7, 1), Trajectory(1, 2)
  )

case class Trajectory(right: Int, down: Int)
case class Position(x: Int, y: Int):
  def followTrajectory(trajectory: Trajectory): Position = Position(x + trajectory.right, y + trajectory.down)
  override def toString: String                          = s"($x,$y)"
object Position:
  val origin: Position = Position(0, 0)
case class State(position: Position, treesHit: Int)

//  val mapWidth = input.head.length
//  val part1Trajectory = 3
//  val part1 = input.foldLeft((Position.origin, 0)){ case ((position, treesHit), treeLine) =>
//    val nextPosition =
//  }
//enum Tile:
//  case Tree, Space
//
//case class Trajectory(right: Int, down: Int)
//
//case class Position(x: Int, y: Int):
//  def followTrajectory(trajectory: Trajectory): Position = Position(x + trajectory.right, y + trajectory.down)
//object Position:
//  val origin: Position = Position(0, 0)
