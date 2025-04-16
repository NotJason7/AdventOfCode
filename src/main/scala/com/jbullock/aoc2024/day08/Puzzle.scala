//package com.jbullock.aoc2024.day08
//
//import scala.annotation.tailrec
//
//@main def solvePuzzle(): Unit =
//  val input = scala.io.Source.fromResource("aoc/2024/Day08/Sample.txt").getLines.toSeq
//  val keys  = input.mkString.toSet.filterNot(_ == '.')
//  val map =
//    for
//      y <- input.indices
//      x <- input.head.indices
//    yield Point(x, y) -> input(y)(x)
//  val validPoints = map.map(_._1)
//  val xMax = validPoints.maxBy(_.x).x
//  val yMax = validPoints.maxBy(_.y).y
//  val antennas      = map.filterNot((_, c) => c == '.')
//  val antennasByKey = keys.map(key => AntennaArray(key, antennas.filter((_, c) => c == key).map(_._1)))
//  println(antennasByKey)
//
//case class AntennaArray(key: Char, antennas: Seq[Point]):
//  def findAntinodes: Seq[Point] = ???
//
//case class AntennaPair(a: Point, b: Point):
//  def antiNodes(xMax: Int, yMax: Int): Seq[Point] =
//    val x = a.x - b.x
//    val y = a.y - b.y
//    val factor = gcf(x, y)
//    val dx = x/factor
//    val dy = y/factor
//    //todo: find the gradient and do y=mx+c
//    
//    
//    
//
//case class Point(x: Int, y: Int):
//  def difference(p: Point): Int = Math.abs(x - p.x) + Math.abs(y - p.y)
//
//@tailrec
//def gcf(a: Int, b: Int): Int =
//  if b == 0 then 1
//  else gcf(b, a % b)
