package com.jbullock.aoc2022.day06

@main def solvePuzzle(): Unit =
  val input                   = io.Source.fromResource("aoc/2022/Day06/Input.txt").mkString.trim
  val startOfPacketMarker     = input.markerIndexEnd(4)
  val realStartOfPacketMarker = input.markerIndexEnd(14)
  println(s"Part 1: $startOfPacketMarker")
  println(s"Part 2: $realStartOfPacketMarker")

extension (s: String)
  def markerIndexEnd(markerSize: Int): Int = s.sliding(markerSize).takeWhile(s => s != s.distinct).size + markerSize
