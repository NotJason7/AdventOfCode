package com.jbullock.aoc2020.day18
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2020/Day18/Input.txt").getLines.toVector
  val basic = Basic
  val part1 = input.map(equation => basic.parseAll(basic.equation, equation).get).sum
  println(s"Part 1: $part1")
  val advanced = Advanced
  val part2    = input.map(equation => advanced.parseAll(advanced.equation, equation).get).sum
  println(s"Part 2: $part2")

trait MathParser extends JavaTokenParsers:
  private val whitespaceRegex: Regex                  = """\s*""".r
  override def skipWhitespace                         = false
  final def whitespace[T](p: => Parser[T]): Parser[T] = whitespaceRegex ~> p <~ whitespaceRegex
  final def number: Parser[Long]                      = wholeNumber ^^ (_.toLong)
  def expression: Parser[Long]                        = number | (whitespace("(") ~> equation <~ whitespace(")"))
  def equation: Parser[Long]

case object Basic extends MathParser:
  override def equation: Parser[Long] =
    expression ~ rep((whitespace("+") ~ expression) | (whitespace("*") ~ expression)) ^^ { case n ~ list =>
      list.foldLeft(n) {
        case (x, "+" ~ y) => x + y
        case (x, "*" ~ y) => x * y
      }
    }

case object Advanced extends MathParser:
  def term: Parser[Long] = expression ~ rep(whitespace("+") ~> expression) ^^ { case x ~ y => y.foldLeft(x)(_ + _) }
  override def equation: Parser[Long] = term ~ rep((whitespace("*") ~> term)) ^^ { case x ~ y => y.foldLeft(x)(_ * _) }
