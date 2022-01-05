package AOC2021

import scala.io.Source
import scala.annotation.tailrec

val input = Source
  .fromFile(
    "C:\\Users\\Jason.Bullock\\Documents\\Git\\advent-of-code\\src\\main\\scala\\AOC2021\\AOC6Input.txt"
  )
  .getLines
  .mkString
  .split(",")
  .filter(!_.isEmpty)
  .map(_.toInt)

def project(current: Map[Int, Long]): Map[Int, Long] =
  Map(
    0 -> current.getOrElse(1, 0L),
    1 -> current.getOrElse(2, 0L),
    2 -> current.getOrElse(3, 0L),
    3 -> current.getOrElse(4, 0L),
    4 -> current.getOrElse(5, 0L),
    5 -> current.getOrElse(6, 0L),
    6 -> (current.getOrElse(7, 0L) + current.getOrElse(0, 0L)),
    7 -> current.getOrElse(8, 0L),
    8 -> current.getOrElse(0, 0L)
  )

@tailrec
def loop(
    demographic: Map[Int, Long],
    ageFunction: Map[Int, Long] => Map[Int, Long],
    steps: Int
): Map[Int, Long] =
  if steps == 0 then demographic
  else loop(ageFunction(demographic), ageFunction, steps - 1)

def population(demographic: Map[Int, Long]): Long =
  demographic.foldLeft(0L) { case (a, (k, v)) => a + v }

@main
def AOC6 =
  // val initialPopulation = List(3, 4, 3, 1, 2)
// val demographic = initialPopulation.groupBy(identity).map((k, v) => (k, v.size))
  val demographic =
    input.groupBy(identity).map((k, v) => (k, v.length.toLong))
  println(demographic)
  val populationSize = population(loop(demographic, project, 256))
  println(populationSize)
