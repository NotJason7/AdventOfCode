package com.jbullock.aoc2016.Day06
import scala.io.Source
@main def solvePuzzleGolf(): Unit =
  val input = Source.fromResource("aoc/2016/Day06/Input.txt").getLines.toVector
  val transposeGroup = input.transpose.map(_.toVector).map(_.groupBy(identity))
  println(transposeGroup.map(_.maxBy(_._2.size)).map(_._1).mkString)
  println(transposeGroup.map(_.minBy(_._2.size)).map(_._1).mkString)
