package com.jbullock.aoc2024.day14

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2024/Day14/Input.txt").getLines().toSeq
  val robots = input.map { case s"p=$x,$y v=$dx,$dy" =>
    Robot(Position(x.toInt, y.toInt), dx.toInt, dy.toInt, Position(x.toInt, y.toInt))
  }
  val gridX              = 101
  val gridY              = 103
  val robotsIn100Seconds = robots.map(_.move(100, gridX, gridY))
  val robotsByQuadrantIn100Seconds = robotsIn100Seconds
    .flatMap(_.quadrant(gridX, gridY))
    .groupBy(identity)
    .view
    .mapValues(v => v.size)
    .values
    .toSeq
  val safetyFactor = robotsByQuadrantIn100Seconds.product
  println(s"Part 1: $safetyFactor")
  val part2 = findLeastRandomArrangement(robots, gridX, gridY)
  println(s"Part 2: $part2")
  val robotsAtTreeTime = robots.map(_.move(part2, gridX, gridY))
  drawGrid(robotsAtTreeTime, gridX, gridY)

//  val part2 = searchForTree(robots, gridX, gridY)
//  println(s"Part 2: $part2")

def findLeastRandomArrangement(robots: Seq[Robot], xMax: Int, yMax: Int): Int =
  val lastFrame = findLongestLoop(robots, xMax, yMax)

  @tailrec def loop(currentBots: Seq[Robot], entropy: Seq[(Int, Int)]): Seq[(Int, Int)] =
    if entropy.size == lastFrame then entropy
    else
      val currentEntropy = calculateEntroy(currentBots)
      val nextEntropy    = entropy :+ (entropy.size, currentEntropy)
      val nextBots       = currentBots.map(_.move(1, xMax, yMax))
      loop(nextBots, nextEntropy)

  val entropyOverTime = loop(robots, Seq.empty[(Int, Int)])
  entropyOverTime.minBy(_._2)._1

def calculateEntroy(robots: Seq[Robot]): Int =
  val xs   = robots.map(_.p.x)
  val avgX = xs.sum / xs.size
  val dxs  = xs.map(x => Math.abs(x - avgX))

  val ys   = robots.map(_.p.y)
  val avgY = ys.sum / ys.size
  val dys  = ys.map(y => Math.abs(y - avgY))
  dxs.sum + dys.sum

def findLongestLoop(robots: Seq[Robot], xMax: Int, yMax: Int): Int =

  @tailrec def loop(currentBots: Seq[Robot], seconds: Int): Int =
    if currentBots.isEmpty then seconds
    else
      val nextBots = currentBots.map(_.move(1, xMax, yMax)).filterNot(_.hasLooped)
      loop(nextBots, seconds + 1)

  loop(robots, 0)

def searchForTree(robots: Seq[Robot], xMax: Int, yMax: Int): Int =

  val tolerance = 2

  @tailrec def loop(currentBots: Seq[Robot], seconds: Int): Int =
    val failsafeSeconds = 10000000
    if seconds % 1000 == 0 then println(s"Checking $seconds seconds...")
    if seconds > failsafeSeconds then
      print("Took too long")
      failsafeSeconds
    else if checkAdjacency(currentBots) > tolerance then
      drawGrid(currentBots, xMax, yMax)
      seconds
    else
      val nextBots = currentBots.map(_.move(1, xMax, yMax))
      loop(nextBots, seconds + 1)

  loop(robots, 0)

def checkAdjacency(robots: Seq[Robot]): Int =
  val robotAdjacencyCount = robots.map(_.p.adjacentPositions.count(p => robots.exists(r => r.p == p)))
  robotAdjacencyCount.max

def drawGrid(robots: Seq[Robot], xMax: Int, yMax: Int): Unit =
  val grid =
    for
      y <- 0 to yMax
      x <- 0 to xMax
      c = if robots.exists(robot => robot.p.x == x && robot.p.y == y) then "X" else " "
    yield c
  grid.mkString.grouped(xMax + 1).toSeq.map(_.mkString).foreach(println)

case class Position(x: Int, y: Int):
  def adjacentPositions: Seq[Position] =
    for
      y <- -1 to 1
      x <- -1 to 1
    yield Position(x, y)

case class Robot(p: Position, dx: Int, dy: Int, initialPosition: Position, hasLooped: Boolean = false):
  def move(moves: Int, xMax: Int, yMax: Int): Robot =
    val rawNextX      = (p.x + (dx * moves)) % xMax
    val nextX         = if rawNextX < 0 then xMax + rawNextX else rawNextX
    val rawNextY      = (p.y + (dy * moves)) % yMax
    val nextY         = if rawNextY < 0 then yMax + rawNextY else rawNextY
    val nextPosition  = Position(nextX, nextY)
    val nextHasLooped = hasLooped || nextPosition == initialPosition
    Robot(nextPosition, dx, dy, initialPosition, nextHasLooped)
  def quadrant(xMax: Int, yMax: Int): Option[Int] =
    val xMid = xMax / 2
    val yMid = yMax / 2
    if p.x == xMid || p.y == yMid then None
    else
      (p.x > xMid, p.y > yMid) match
        case (false, false) => Some(0)
        case (true, false)  => Some(1)
        case (false, true)  => Some(2)
        case (true, true)   => Some(3)
