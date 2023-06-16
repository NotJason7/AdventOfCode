package com.jbullock.aoc2020.day04

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2020/Day04/Input.txt").getLines.toVector
  val passports = input
    .foldLeft(Vector("")) { case (docs: Vector[String], line: String) =>
      if line.isEmpty then docs.appended("")
      else docs.dropRight(1).appended(if docs.last.isEmpty then line else docs.last ++ " " ++ line)
    }
    .map(_.split(" ").map { case s"$key:$value" => key -> value }.toMap)
    .flatMap(Passport.fromMap)
  println(s"Part 1: ${passports.size}")
  println(s"Part 2: ${passports.count(_.isValid)}")

extension (i: Int) def isBetween(a: Int, b: Int): Boolean = a <= i && i <= b

case class Passport(
    birthYear: Year,
    issueYear: Year,
    expirationYear: Year,
    height: Height,
    hairColor: HexColor,
    eyeColor: Color,
    passportId: PassportId
):
  def isValid: Boolean = birthYear.isValidBetween(1920, 2002) && issueYear.isValidBetween(2010, 2020) && expirationYear
    .isValidBetween(2020, 2030) && height.isValid && hairColor.isValid && eyeColor.isValid && passportId.isValid
object Passport:
  def fromMap(m: Map[String, String]): Option[Passport] =
    for
      birth      <- m.get("byr").map(Year.apply)
      issue      <- m.get("iyr").map(Year.apply)
      expiration <- m.get("eyr").map(Year.apply)
      height     <- m.get("hgt").map(Height.apply)
      hairColor  <- m.get("hcl").map(HexColor.apply)
      eyeColor   <- m.get("ecl").map(Color.apply)
      passportId <- m.get("pid").map(PassportId.apply)
    yield Passport(birth, issue, expiration, height, hairColor, eyeColor, passportId)

case class Year(value: String):
  def isValidBetween(start: Int, end: Int): Boolean = value.toIntOption.exists(_.isBetween(start, end))
case class Height(value: String):
  def isValid: Boolean = value match
    case s"${n}in" if n.toInt.isBetween(59, 76)   => true
    case s"${n}cm" if n.toInt.isBetween(150, 193) => true
    case _                                        => false
case class HexColor(value: String):
  def isValid: Boolean =
    value.head == '#' && value.tail.length == 6 && value.tail.forall((('0' to '9') ++ ('a' to 'f')).contains)
case class Color(value: String):
  def isValid: Boolean = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)
case class PassportId(value: String):
  def isValid: Boolean = value.length == 9 && value.forall(('0' to '9').contains)
