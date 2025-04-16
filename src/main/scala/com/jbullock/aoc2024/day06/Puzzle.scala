//package com.jbullock.aoc2024.day06
//
//import scala.annotation.tailrec
//
//@main def solvePuzzle(): Unit =
//  val input = scala.io.Source.fromResource("aoc/2024/Day06/Sample.txt").getLines.toSeq
//  val map =
//    (for
//      y <- input.indices
//      x <- input.head.indices
//    yield Position(x, y) -> input(y)(x)).toMap
//  val guardStart = map.filter((_, c) => c == '^').keys.head
//  val fixedMap   = map + (guardStart -> '.')
//  val guard      = Guard(Up, guardStart)
//  val guardPath  = guard.findPath(fixedMap).getOrElse(Seq.empty[Position])
//  val part1      = guardPath.toSet.size
//  println(s"Part 1: $part1")
//  val potentialObstructions = guardPath.toSet - guardStart
//  val loopCausingObstructions = potentialObstructions.filter{ obstruction =>
//    val loopingHeadings = fixedMap.findLoopingHeadings(obstruction)
//    if guardPath.toSet
//  }
////  val obstructionMaps         = potentialObstructions.map(obstruction => fixedMap + (obstruction -> '#'))
////  val loopCausingObstructions = obstructionMaps.filter(map => guard.findPath(map).isEmpty)
////  val part2                   = loopCausingObstructions.size
////  println(s"Part 2: $part2")
//
//extension (m: Map[Position, Char])
//  def findLoopingHeadings(o: Position): Set[Heading] =
//    val updatedMap = m + (o -> '#')
//    val potentialApproaches = Seq(
//      Heading(Up, o.move(Down)),
//      Heading(Down, o.move(Up)),
//      Heading(Left, o.move(Right)),
//      Heading(Right, o.move(Left))
//    )
//    potentialApproaches.flatMap(heading => heading.findLoop(updatedMap)).flatten.toSet
//
//sealed trait Direction(val right: Direction, val x: Int, val y: Int)
//case object Up    extends Direction(Right, 0, -1)
//case object Right extends Direction(Down, 1, 0)
//case object Down  extends Direction(Left, 0, 1)
//case object Left  extends Direction(Up, -1, 0)
//
//case class Position(x: Int, y: Int):
//  def move(d: Direction): Position = Position(x + d.x, y + d.y)
//
//case class Heading(direction: Direction, position: Position):
//  def move: Heading = Heading(direction, position.move(direction))
//  def turn: Heading = Heading(direction.right, position)
//  def findLoop(map: Map[Position, Char]): Option[Seq[Heading]] =
//    @tailrec def loop(heading: Heading = this, covered: Seq[Heading] = Seq.empty[Heading]): Option[Seq[Heading]] =
//      val nextHeading = heading.move
//      val destination = map.getOrElse(nextHeading.position, ' ')
//      if covered.contains(nextHeading) then Some(covered)
//      else
//        destination match
//          case '#' => loop(heading.turn, covered)
//          case '.' => loop(nextHeading, covered :+ heading)
//          case _   => None
//    loop()
//
//  def findPath(map: Map[Position, Char]): Option[Seq[Heading]] =
//    @tailrec def loop(current: Heading, covered: Seq[Heading] = Seq.empty[Heading]): Option[Seq[Heading]] =
//      val nextHeading = current.move
//      val destination = map.getOrElse(nextHeading, ' ')
//      if covered.contains(nextHeading) then None
//      else
//        destination match
//          case '#' => loop(heading.right, current, covered)
//          case '.' => loop(heading, nextPosition, covered :+ (heading, current))
//          case ' ' => Some(covered.map(_._2) :+ current)
//
//    loop(heading, currentPosition)
//
//case class Guard(heading: Direction, currentPosition: Position):
//
//  def findPath(map: Map[Position, Char]): Option[Seq[Position]] =
//
//    @tailrec def loop(
//        heading: Direction,
//        current: Position,
//        covered: Seq[(Direction, Position)] = Seq.empty[(Direction, Position)]
//    ): Option[Seq[Position]] =
//      val nextPosition = current.move(heading)
//      val destination  = map.getOrElse(nextPosition, ' ')
//      if covered.contains((heading, current)) then None
//      else
//        destination match
//          case '#' => loop(heading.right, current, covered)
//          case '.' => loop(heading, nextPosition, covered :+ (heading, current))
//          case ' ' => Some(covered.map(_._2) :+ current)
//
//    loop(heading, currentPosition)
