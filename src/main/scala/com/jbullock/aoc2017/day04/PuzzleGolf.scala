package com.jbullock.aoc2017.day04
@main def run = List(scala.io.Source.fromResource("aoc/2017/Day04/Input.txt").getLines.toList.map(_.split(" ")
  .toList)).flatMap(p => List(p, p.map(_.map(_.sorted)))).map(_.count(v => v == v.distinct)).foreach(println)
