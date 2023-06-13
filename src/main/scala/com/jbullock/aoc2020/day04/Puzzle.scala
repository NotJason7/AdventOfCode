package com.jbullock.aoc2020.day04

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2020/Day04/Input.txt").getLines.toVector
  val documents = input
    .foldLeft(Vector("")) { case (docs: Vector[String], line: String) =>
      if line.isEmpty then docs.appended("")
      else
        docs.dropRight(1).appended {
          if docs.last.isEmpty then line else docs.last ++ " " ++ line
        }
    }
    .map { documentString =>
      documentString.split(" ").map { case s"$key:$value" => key -> value }.toMap
    }
  val requiredFields = Vector("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val part1          = documents.count(map => isValidPassport(requiredFields, map))
  println(s"Part 1: $part1")

def isValidPassport(requiredFields: Vector[String], map: Map[String, String]): Boolean =
  requiredFields.forall(map.contains)

def isStricterValidPassport(requiredFields:)

case class Passport(
   birthYear: BirthYear,
   issueYear: IssueYear,
   expirationYear: Int,
   height: (Int, UnitOfMeasure),
   hairColor: HexColor,
   eyeColor: EyeColor,
   passportId: PassportId,
   counrtyId: Option[String]


                   )

enum Field(val shortName: String):
  case BirthYear extends Field("byr")
  case IssueYear extends Field("iyr")
  case ExpirationYear extends Field("eyr")
  case Height extends Field("hgt")
  case HairColor extends Field("hcl")
  case EyeColor extends Field("ecl")
  case PassportId extends Field("pid")
  case CountryId extends Field("cid")
