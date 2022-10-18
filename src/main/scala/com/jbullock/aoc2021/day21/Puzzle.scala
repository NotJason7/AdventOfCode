package com.jbullock.aoc2021.day21

import scala.io.Source

@main
def solvePuzzle(): Unit =
  val p = ".#/#.".toPattern
//  println(p)
//  println(p.rotateRight)
//  println(p.flip)
  val test = Vector((1 to 4).toVector, (5 to 8).toVector, (9 to 12).toVector, (13 to 16).toVector)
  println(test.transpose.map(_.grouped(2).toVector).transpose)

type Pixel = Boolean
type Pattern = Vector[Vector[Pixel]]

extension(p: Pattern)
  def rotateRight: Pattern = p.transpose.flip
  def flip: Pattern = p.map(_.reverse)
  def divide: Vector[Pattern] = ???


extension(s: String)
  def toPattern: Pattern = s.split("/").toVector.map(_.toVector.map(_ == '#'))
