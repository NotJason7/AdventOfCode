package AOC2021

import scala.io.Source

val question = "AOC5"
val fileName =
  s"C:\\Users\\Jason.Bullock\\Documents\\Git\\advent-of-code\\src\\main\\scala\\AOC2021\\${question}Input.txt"
val input = Source
  .fromFile(fileName)
  .getLines
  .mkString

val testInput = Map(
  (0, 9) -> (5, 9),
  (8, 0) -> (0, 8),
  (9, 4) -> (3, 4),
  (2, 2) -> (2, 1),
  (7, 0) -> (7, 4),
  (6, 4) -> (2, 0),
  (0, 9) -> (2, 9),
  (3, 4) -> (1, 4),
  (0, 0) -> (8, 8),
  (5, 5) -> (8, 2)
)

val filteredTestInput = testInput.filter((k, v) => k._1 == v._1 || k._2 == v._2)

val expandedfilteredInput = filteredTestInput.toList.map { (k, v) =>
  if k._1 == v._1 then
    Range(List(k._2, v._2).min, List(k._2, v._2).max + 1).toList.map(x =>
      (k._1, x)
    )
  else
    Range(List(k._1, v._1).min, List(k._1, v._1).max + 1).toList.map(x =>
      (x, k._2)
    )
}

def expand(input: Map[(Int, Int), (Int, Int)]): List[(Int, Int)] =
  val endPoints = input.toList.map { (k, v) =>
    val expanded = if k._1 == v._1 then
      val range = List(k._2, v._2)
      Range(range.min, range.max + 1).toList.map(x => (k._1, x)).flatten
    else
      val range = List(k._1, v._1)
      Range(range.min, range.max + 1).toList.map(x => (x, k._2)).flatten
  }

@main
def AOC5 =
  ???
