package com.jbullock.aoc2023.day08

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input        = scala.io.Source.fromResource("aoc/2023/Day08/Input.txt").getLines.toVector
  val instructions = input.head
  given mapping: Map[String, (String, String)] =
    input.drop(2).map { case s"$key = ($left, $right)" => key -> (left, right) }.toMap
  val start = "AAA"
  val end   = "ZZZ"
  val part1 = countSteps(start, end, instructions)
  println(s"Part 1: $part1")
  val starts = mapping.keys.filter(_.endsWith("A")).toVector
  val loops  = starts.map(start => findLoop(start, instructions)).map(x => BigInt(x.loopLength))
  val part2 = lowestCommonMultiple(loops)
  println(s"Part 2: $part2")

extension [A](i: Iterator[A])
  def takeUntil(predicate: A => Boolean): Vector[A] =
    @tailrec def loop(asSoFar: Vector[A]): Vector[A] =
      val nextVal = i.next
      val nextAs  = asSoFar :+ nextVal
      if predicate(nextVal) then nextAs else loop(nextAs)
    loop(Vector.empty[A])

def countSteps(start: String, end: String, instructions: String)(using
    mapping: Map[String, (String, String)]
): Int =
  val loopedInstructions = LazyList.continually(instructions).flatten.iterator
  @tailrec def loop(current: String, stepsSoFar: Int): Int =
    if current == end then stepsSoFar
    else
      val mapped          = mapping.getOrElse(current, ("ZZZ", "ZZZ"))
      val nextInstruction = loopedInstructions.take(1).toVector.head
      val next            = if nextInstruction == 'L' then mapped._1 else mapped._2
      loop(next, stepsSoFar + 1)
  loop(start, 0)

def lowestCommonMultiple(x: BigInt, y: BigInt): BigInt =
  @tailrec def sync(a: BigInt, b: BigInt): BigInt =
    if a == b then a
    else if a > b then sync(a, b + y)
    else sync(a + x, b)
  sync(x, y)

def lowestCommonMultiple(values: Vector[BigInt]): BigInt =
  val sortedValues = values.sortWith(_ >= _)
  @tailrec def loop(leftToSync: Vector[BigInt]): BigInt =
    if leftToSync.length == 1 then leftToSync.head
    else
      val Vector(a, b) = leftToSync.take(2)
      val synced       = lowestCommonMultiple(a, b)
      loop(synced +: leftToSync.drop(2))
  loop(sortedValues)

//def lowestCommonMultiple(loops: Vector[Loop]): BigInt =
//  val biggestLoop               = loops.maxBy(_.loopLength)
//  val biggestLoopAfterFirstStep = biggestLoop.toIterator.drop(1)
//  val smallerLoops              = loops.filterNot(_ == biggestLoop).map(_.toIterator)
//  @tailrec def loop: BigInt =
//    val biggestValues = biggestLoopAfterFirstStep.next.toSet
//    val nextSmaller =
//      smallerLoops.map(_.takeUntil((bigInts: Vector[BigInt]) => bigInts.max >= biggestValues.max).flatten.toSet)
////    println(s"Comparing $biggestValues to ${nextSmaller.mkString(",")}")
//    nextSmaller.reduce(_.intersect(_)).intersect(biggestValues).minOption match
//      case Some(min) => min
//      case None      => loop
//  loop

case class Loop(startNode: String, startIndex: Int, loopLength: Int, pathToLoop: Vector[(String, Int)]):
  override def toString: String =
    s"Loop(starting at ${pathsEndingInZ.head} and every $loopLength after)"
  val pathsEndingInZ: Vector[BigInt] =
    pathToLoop.map(_._1).zipWithIndex.filter { case (string, index) => string.endsWith("Z") }.map(x => BigInt(x._2))
  def toIterator: Iterator[Vector[BigInt]] =
    LazyList
      .iterate(pathsEndingInZ)((bigInts: Vector[BigInt]) => bigInts.map(bi => bi + BigInt(loopLength)))
      .iterator

def findLoop(start: String, instructions: String)(using
    mapping: Map[String, (String, String)]
): Loop =
  val loopedInstructionsWithIndex = LazyList.continually(instructions.zipWithIndex).flatten.iterator
  @tailrec def loop(stepsSoFar: Vector[(String, Int)]): Loop =
    val currentNode                  = stepsSoFar.last._1
    val nextNodeChoices              = mapping(currentNode)
    val (nextInstruction, nextIndex) = loopedInstructionsWithIndex.next
    val nextNode                     = if nextInstruction == 'L' then nextNodeChoices._1 else nextNodeChoices._2
    val nextStep                     = (nextNode, nextIndex)
    if stepsSoFar.contains(nextStep) then
      val startNode  = nextStep._1
      val startIndex = stepsSoFar.indexOf(nextStep)
      val loopLength = stepsSoFar.length - startIndex
      Loop(startNode, startIndex, loopLength, stepsSoFar :+ nextStep)
    else loop(stepsSoFar :+ nextStep)
  val startingStep = Vector((start, 0))
  loop(startingStep)

//def countStepsParallel(starts: Vector[String], end: String => Boolean, instructions: String)(using
//    mapping: Map[String, (String, String)]
//): Int =
//  val loopedInstructions = LazyList.continually(instructions).flatten.iterator
//  @tailrec def loop(currents: Vector[String], stepsSoFar: Int): Int =
//    if currents.exists(end) then println(s"$stepsSoFar steps: $currents")
//    if currents.forall(end) then stepsSoFar
//    else
//      val mapped          = currents.map(current => mapping.getOrElse(current, ("ZZZ", "ZZZ")))
//      val nextInstruction = loopedInstructions.take(1).toVector.head
//      val nexts           = if nextInstruction == 'L' then mapped.map(_._1) else mapped.map(_._2)
//      loop(nexts, stepsSoFar + 1)
//  loop(starts, 0)
