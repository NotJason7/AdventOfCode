package com.jbullock.aoc2017.day12

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  println(s"Part 1: ${Puzzle.networks.filter(_.contains(0)).head.size}")
  println(s"Part 2: ${Puzzle.networks.length}")

object Puzzle:
  val input: Vector[String] = Source.fromResource("aoc/2017/Day12/Input.txt").getLines.toVector
  val networks: Seq[Set[Int]] = input.map("\\d+".r.findAllIn(_).toSet.map(_.toInt)).combineOverlappingSets

extension(s: Seq[Set[Int]])
  def combineOverlappingSets: Seq[Set[Int]] =
    @tailrec
    def loop(sets: Seq[Set[Int]]): Seq[Set[Int]] =
      val collapsed = sets.foldLeft(Vector.empty[Set[Int]]){ (v, p) =>
        v.filterNot(_.intersect(p).isEmpty).headOption match
          case Some(join: Set[Int]) => v.updated(v.indexOf(join), join ++ p)
          case None => v :+ p
      }
      if collapsed.size == sets.size then sets else loop(collapsed)
    loop(s)
