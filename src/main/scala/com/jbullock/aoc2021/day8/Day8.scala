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

Unique Lengths:
1: __c__f_ (2)
4: _bcd_f_ (4)
7: a_c__f_ (3)
8: abcdefg (7)

Length 5s:
2: a_cde_g
3: a_cd_fg
5: ab_d_fg

Length 6s:
0: abc_efg
6: ab_defg
9: abcd_fg

3 is uniquely a superset of 1 with 5 members:
1: __c__f_
3: a_cd_fg

x: 4-1
x: _b_d___ -> is subset of 5 so
5 is uniquely a superset of (4-1) and has 5 members:
5: ab_d_fg

2 has 5 members and is not 3 or 5:
2: a_cde_g

9 has 6 members and is a superset of 3:
9: abc_efg

0 has 6 members, is not 9, superset of 1:
0: a_cdefg

6 has 6 members, is not 0 or 9:
6: abcd_fg

*/

@main
def runDay8(): Unit =
  println(s"Part one: ${Day8.part1()}")
  println(s"Part two: ${Day8.part2()}")

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


  def part1(): Int =
    val wantedIntegers = List(1,4,7,8)
    val decodedInput = input.map{scrambledDisplay =>
      val decoder = decodeMap(scrambledDisplay.cipher)
      val plainText = scrambledDisplay.encrypted.map(decoder.get(_))
      plainText
    }.map(_.map{
        case Some(x) => x
        case _ => None
      }.filter(wantedIntegers.contains(_))
    )
    decodedInput.flatten.size
    
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

    // length 5 deriving
    val lengthFives = cipher.filter(_.size == 5)
    // 3: 5 members, superset of 1
    val three = lengthFives.filter(one.subsetOf(_)).head
    // 5: 5 members, superset of (4-1)
    val five = lengthFives.filter((four -- one).subsetOf(_)).head
    // 2: 5 members, not 3 or 5
    val two = lengthFives.filterNot(x => x == three || x == five).head

    // length 6 deriving
    val lengthSixes = cipher.filter(_.size == 6)
    // 9: 6 members, superset of 3
    val nine = lengthSixes.filter(three.subsetOf(_)).head
    // 0: 6 members, not 9, superset of 1
    val zero = lengthSixes.filterNot(_ == nine).filter(one.subsetOf(_)).head
    // 6: 6 members, not 0 or 9
    val six = lengthSixes.filterNot(x => x == zero || x == nine).head

    // Put this into a map to cleanly decode the answer
    Map(
      zero -> 0,
      one -> 1,
      two -> 2,
      three -> 3,
      four -> 4,
      five -> 5,
      six -> 6,
      seven -> 7,
      eight -> 8,
      nine -> 9
    )

}
