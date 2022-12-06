package com.jbullock.aoc2022.day06

@main def solvePuzzle(): Unit =
  val input         = io.Source.fromResource("aoc/2022/Day06/Input.txt").mkString.trim
  val packetMarker  = input.markerIndex(4)
  val messageMarker = input.markerIndex(14)
  println(s"Part 1: $packetMarker")
  println(s"Part 2: $messageMarker")

extension (s: String)
  def markerIndex(markerSize: Int): Int = s.sliding(markerSize).takeWhile(s => s != s.distinct).size + markerSize
