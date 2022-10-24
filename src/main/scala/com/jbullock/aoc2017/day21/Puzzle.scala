package com.jbullock.aoc2017.day21

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  val start = Pattern.fromString(".#./..#/###")
//  val start = Pattern.fromString("../..")
  val rules = Source.fromResource("aoc/2017/Day21/Input.txt").getLines.toVector
    .flatMap(_.split(" => ").grouped(2).map(v => Pattern.fromString(v(0)) -> Pattern.fromString(v(1)))).toMap
  val end = iterate(5, start, rules)
//  val total = end.map(_.pixels.map(x => x.flatMap(y => if y then 1 else 0)))
//  println(total)
//  val total = end.map(p => p.pixels.flatten.map(pix => if pix then 1 else 0).sum).sum
//  val count = end.map(p => p.pixels.count(_))
//  println(total)

def iterate(n: Int, start: Pattern, rules: Map[Pattern, Pattern]): Vector[Pattern] =
  @tailrec def loop(n: Int, patterns: Vector[Pattern]): Vector[Pattern] =
    println(patterns)
    println
    if n == 0 then patterns else
      loop(n - 1, patterns.flatMap(_.enhance(rules)))
  loop(n, Vector(start))

case class Pattern(pixels: Vector[Vector[Boolean]]):
  def rotate: Pattern = Pattern(
    (for {
      x <- pixels.indices
      y <- pixels.indices
    } yield pixels(pixels.size - 1 - y)(x)).grouped(pixels.size).toVector.map(_.toVector))
  def rotateN(n: Int): Pattern =
    @tailrec def loop(n: Int, p: Pattern): Pattern =
      if n == 0 then p
      else loop(n - 1, p.rotate)
    loop(n, this)

  def flipX: Pattern = Pattern(pixels.map(_.reverse))
  def flipY: Pattern = Pattern(pixels.transpose.map(_.reverse).transpose)
  def split: Vector[Pattern] = pixels.size match
    case 4 => (for {
      x <- 0 to 1
      y <- 0 to 1
    } yield Vector(
      Vector(pixels(0 + (x * 2))(0 + (y * 2)), pixels(0 + (x * 2))(1 + (y * 2))),
      Vector(pixels(1 + (x * 2))(0 + (y * 2)), pixels(1 + (x * 2))(1 + (y * 2))))).toVector.map(Pattern.apply)
    case _ => throw new RuntimeException("Unable to split unless it's a 4x4 pattern")
  def variations: Vector[Pattern] = (0 to 3).toVector.map(this.rotateN).flatMap(x => Vector(x, x.flipX, x.flipY)).distinct
  def enhance(rules: Map[Pattern, Pattern]): Vector[Pattern] =
    pixels.size match
      case 4 => this.split.flatMap(_.enhance(rules))
      case _ => variations.find(rules.contains) match
        case None => throw new RuntimeException("Pattern was not contained in the rules")
        case Some(p) => Vector(rules(p))

  override def toString: String = pixels.map(_.map(b => if b then '#' else '.').mkString).mkString("\n")

object Pattern:
  def fromString(s: String): Pattern = Pattern(s.split("/").toVector.map(_.toVector.map(_ == '#')))
