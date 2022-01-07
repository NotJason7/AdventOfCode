package com.jbullock.aoc2021.day8

import scala.io.Source

/*
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

Unique length:
1: __c__f_
4: _bcd_f_
7: a_c__f_
8: abcdefg

5 length:
3: a_cd_fg (5 members, superset of 1)
5: ab_d_fg (5 members, superset of (4-1))
2: a_cde_g (5 members, not equal to 3 or 5)

6 length:
0: abc_efg (6 members, is not 9, superset of 1)
9: abcd_fg (6 members, superset of 3)
6: ab_defg (6 members, not equal to 0 or 9)

derive:
1, 4, 7, 8 are unique
1 + 4 -> bd -> 5 has 5 elements, contains bd
5 contains f -> 3 is only different from 5 in one place
5 + 3 -> 2 is the remaining 5 marker
2 -> 0 is 2 with 1 extra
0 -> 2 minus the 0 letters gives us an identifying 6 character
9 is leftover
*/

@main
def runDay8(): Unit =
  // println(s"Part one: ${Day8.part1()}")
  println(s"Part two: ${Day8.part2()}")
  // Day8.part2()

object Day8 {
  val input = Source
    .fromResource("2021/Day8Input.txt")
    .getLines
    .toList
    .map(_.replace(" | ", ","))
    .map(_.split(",").toList.map(_.split(" ").toList))
    .map{line =>
      ScrambledDisplay(line(0).map(_.toSet), line(1).map(_.toSet))
    }

  case class ScrambledDisplay(cipher: List[Set[Char]], encrypted: List[Set[Char]])


  // def part1(): Int =
  //   val wantedIntegers = List(1,4,7,8)
  //   val decodedInput = input.map{scrambledDisplay =>
  //     val decoder = decodeMap(scrambledDisplay.cipher)
  //     val plainText = scrambledDisplay.encrypted.map(decoder.get(_))
  //     plainText
  //   }.map(_.map{
  //       case Some(x) => x
  //       case _ => None
  //     }.filter(wantedIntegers.contains(_))
  //   )
  //   decodedInput.flatten.size
    
  def part2(): Int =
    val decodedInput = input.map{scrambledDisplay =>
      val decoder = decodeMap(scrambledDisplay.cipher)
      scrambledDisplay.encrypted.map(decoder.get(_))
    }.map(_.map{
        case Some(x) => x.toString
        case None => None
      }.foldLeft("")(_ + _).toInt
    )
    decodedInput.sum
    

  def decodeMap(cipher: List[Set[Char]]): Map[Set[Char], Int] =
    // 1, 4, 7, 8 are unique lengths
    val one = cipher.filter(_.size == 2).head
    val four = cipher.filter(_.size == 4).head
    val seven = cipher.filter(_.size == 3).head
    val eight = cipher.filter(_.size == 7).head
    
    println(s"one = $one")
    println(s"four = $four")
    println(s"seven = $seven")
    println(s"eight = $eight")

    // length 5 deriving
    val lengthFives = cipher.filter(_.size == 5)
    // 3: 5 members, superset of 1
    val three = lengthFives.filter(one.subsetOf(_)).head
    println(s"three = $three")

    // 5: 5 members, superset of (4-1)
    val five = lengthFives.filter((four -- one).subsetOf(_)).head
    println(s"five = $five")

    // 2: 5 members, not 3 or 5
    val two = lengthFives.filterNot(x => x == three || x == five).head
    println(s"two = $two")

    // length 6 deriving
    val lengthSixes = cipher.filter(_.size == 6)
    
    // 9: 6 members, superset of 3
    val nine = lengthSixes.filter(three.subsetOf(_)).head
    println(s"nine = $nine")

    // 0: 6 members, not 9, superset of 1
    val zero = lengthSixes.filterNot(_ == nine).filter(one.subsetOf(_)).head
    println(s"zero = $zero")

    // 6: 6 members, not 0 or 9
    val six = lengthSixes.filterNot(x => x == zero || x == nine).head
    println(s"six = $six")

    // Put this into a map to cleanly decode the answer
    val output = Map(
      zero -> 0,
      one -> 1,
      three -> 2,
      two -> 3,
      four -> 4,
      five -> 5,
      six -> 6,
      seven -> 7,
      eight -> 8,
      nine -> 9
    )
    println(output)
    output

}
