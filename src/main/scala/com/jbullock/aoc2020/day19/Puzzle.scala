package com.jbullock.aoc2020.day19

import scala.annotation.tailrec
import scala.util.matching.Regex

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2020/Day19/Sample2.txt").getLines.toVector
  val (messages, rules) =
    input.filter(_.nonEmpty).partition(message => message.startsWith("a") || message.startsWith("b"))
  val parsedRules =
    rules.map { case s"$id: $ruleString" => id.toInt -> Rule.fromString(ruleString) }.toMap
  val part1Regex = State(parsedRules).toRegexString
  val part1      = messages.count(_.matches(part1Regex))
  println(s"Part 1: $part1")
  val part2Doubles = Math.ceil(messages.map(_.length).max.toDouble / 2).toInt
  val part2RuleUpdates =
    Map(8 -> Rules(oneOrMore(42)), 11 -> oneToNAThenB(part2Doubles, Reference(42), Reference(31)))
  val part2Rules = parsedRules ++ part2RuleUpdates
  val part2Regex = State(part2Rules).toRegexString
  println(part2Regex)
//  val part2      = messages.count(_.matches(part2Regex))
//  println(s"Part 2: $part2")

def oneOrMore(id: Int): Vector[Rule]                  = Vector(Literal("("), Reference(id), Literal(")"), Literal("+"))
def nOfAThenB(n: Int, a: Rule, b: Rule): Vector[Rule] = ((1 to n).map(_ => a) ++ (1 to n).map(_ => b)).toVector
def oneToNAThenB(n: Int, a: Rule, b: Rule): Rule =
  @tailrec def loop(x: Int, rulesSoFar: Rules): Rule =
    if x == 0 then Rules(Vector(Literal(" ( ")) ++ rulesSoFar.references ++ Vector(Literal(" ) ")))
    else
      val nextRules = rulesSoFar.references ++ Vector(Literal("|"), Literal("(")) ++ nOfAThenB(x, a, b) :+ Literal(")")
      loop(x - 1, Rules(nextRules))
  loop(n - 1, Rules(Vector(Literal("(")) ++ nOfAThenB(n, a, b) ++ Vector(Literal(")"))))

case class State(ruleMap: Map[Int, Rule]):
  def toRegexString: String = s"^${resolveRules.ruleMap(0).toString}$$"
  private def next: State =
    val nextMap = ruleMap.map { case (id: Int, rule: Rule) => id -> rule.resolve(ruleMap) }
    State(nextMap)
  @tailrec private final def resolveRules: State =
    if ruleMap.values.forall(_.isResolved) then this else next.resolveRules

trait Rule:
  def isResolved: Boolean
  def resolve(rules: Map[Int, Rule]): Rule
object Rule:
  def fromString(s: String): Rule =
    s.replaceAll("\"", "") match
      case c if c == "a" | c == "b" => Literal(c)
      case s"$left | $right"        => Choice(Rule.fromString(left), Rule.fromString(right))
      case id if !id.contains(" ")  => Reference(id.toInt)
      case referencedIds            => Rules(referencedIds.split(" ").toVector.map(Rule.fromString))
case class Literal(string: String) extends Rule:
  override def isResolved: Boolean                  = true
  override def resolve(rules: Map[Int, Rule]): Rule = this
  override def toString: String                     = string
case class Choice(left: Rule, right: Rule) extends Rule:
  override def isResolved: Boolean                  = left.isResolved && right.isResolved
  override def resolve(rules: Map[Int, Rule]): Rule = Choice(left.resolve(rules), right.resolve(rules))
  override def toString: String                     = s"($left|$right)"
case class Reference(referencedId: Int) extends Rule:
  override def isResolved: Boolean                  = false
  override def resolve(rules: Map[Int, Rule]): Rule = rules(referencedId)
case class Rules(references: Vector[Rule]) extends Rule:
  override def isResolved: Boolean                  = references.forall(_.isResolved)
  override def resolve(rules: Map[Int, Rule]): Rule = Rules(references.map(_.resolve(rules)))
  override def toString: String                     = references.map(_.toString).mkString
