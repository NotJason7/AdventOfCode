import scala.io.Source
import scala.annotation.tailrec

val input = Source
  .fromFile(
    "C:\\Users\\Jason.Bullock\\Documents\\Git\\advent-of-code\\src\\main\\scala\\AOC2021\\AOC3Input.txt"
  )
  .getLines
  .toList
  .map(_.toString)

val processedInput = input
  .map(_.toList)
  .transpose
  .map(l => l.map(_.toString.toDouble))
  .map(x => x.sum / x.length)
  .map(x => x.round.toString)

def binaryStringToInt(input: List[String]): Int =
  Integer.parseInt(input.fold("")(_ + _), 2)

val gamma = binaryStringToInt(processedInput)
val epsilon = Integer.parseInt(
  processedInput.map(x => (1 - x.toInt).toString).fold("")(_ + _),
  2
)

def processInput(binaryList: List[String]): List[Int] =
  binaryList
    .map(_.toList)
    .transpose
    .map(l => l.map(_.toString.toDouble))
    .map(x => x.sum / x.length)
    .map(x => x.round.toInt)

def calcOxygen(input: List[String]): Int =

  @tailrec
  def loop(index: Int, remaining: List[String]): List[String] =
    if remaining.length == 1 then remaining
    else
      val processedInput = processInput(remaining)
      val filteredRemaining =
        remaining.filter(x =>
          x(index).toString != processedInput(index).toString
        )
      loop(index + 1, filteredRemaining)

  val binaryString = loop(0, input)
  binaryStringToInt(binaryString)

def calcC02(input: List[String]): Int =

  @tailrec
  def loop(index: Int, remaining: List[String]): List[String] =
    if remaining.length == 1 then remaining
    else
      val processedInput = processInput(remaining)
      val filteredRemaining =
        remaining.filter(x =>
          x(index).toString != (1 - processedInput(index).toInt).toString
        )
      loop(index + 1, filteredRemaining)

  val binaryString = loop(0, input)
  binaryStringToInt(binaryString)

val oxygen = calcOxygen(input)
val c02 = calcC02(input)

@main
def aoc2021_3() =
  println(
    s"Gamma: [$gamma], Epsilon: [$epsilon]. Total power consumption: ${gamma * epsilon}"
  )
  println(
    s"Oxygen: [$oxygen], C02: [$c02]. Life support rating: ${oxygen * c02}"
  )
// val inputLists = input.toList.map(_.toInt)
// println(inputLists)
