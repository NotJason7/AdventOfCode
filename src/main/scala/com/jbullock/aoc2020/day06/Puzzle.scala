package com.jbullock.aoc2020.day06

@main def solvePuzzle(): Unit =
  val input        = scala.io.Source.fromResource("aoc/2020/Day06/Input.txt").getLines.toVector
  val surveyGroups = input.mkString(",").split(",,").toVector.map(_.split(',').map(_.toSet))
  println(s"Part 1: ${surveyGroups.map(_.reduce(_ | _).size).sum}")
  println(s"Part 2: ${surveyGroups.map(_.reduce(_ & _).size).sum}")
