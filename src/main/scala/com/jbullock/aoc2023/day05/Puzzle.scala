package com.jbullock.aoc2023.day05

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input    = scala.io.Source.fromResource("aoc/2023/Day05/Input.txt").getLines.toVector
  val seeds    = input.head.drop(7).split(' ').toVector.map(i => BigInt(i))
  val maps     = input.drop(2).filter(_.nonEmpty)
  val mappings = createMappings(maps)
  val locationIds = mappings.foldLeft(seeds: Vector[BigInt]) {
    case (currentState: Vector[BigInt], mapping: InsaneMapping) =>
      currentState.map(bi => mapping.mapValueToDestination(bi))
  }
  val part1 = locationIds.min
  println(s"Part 1: $part1")

  val seedRanges: Vector[Range] =
    seeds.grouped(2).toVector.map { case Vector(start: BigInt, length: BigInt) => Range(start, start + length) }
  val locationIdsPart2 = mappings.map(_.toSaneMapping).foldLeft(seedRanges) { case (ranges, mapping) =>
    ranges.flatMap(_.applyMapping(mapping))
  }
  val part2 = locationIdsPart2.map(_.start).min
  println(s"Part 2: $part2")

def createMappings(input: Vector[String]) =

  @tailrec def loop(
      lines: Vector[String],
      currentMapping: Option[InsaneMapping],
      finishedMappings: Vector[InsaneMapping]
  ): Vector[InsaneMapping] =
    lines.headOption match
      case Some(line) =>
        line match
          case s"$source-to-$destination map:" =>
            val nextFinished = currentMapping match
              case Some(finished) => finishedMappings :+ finished
              case None           => finishedMappings
            val nextMapping = Some(InsaneMapping(Vector.empty[InsaneRule]))
            loop(lines.drop(1), nextMapping, nextFinished)
          case s"$destinationRangeStart $sourceRangeStart $rangeLength" =>
            val rule =
              InsaneRule(BigInt(destinationRangeStart), BigInt(sourceRangeStart), BigInt(rangeLength))
            val nextMapping = currentMapping.map(_.addRule(rule))
            loop(lines.drop(1), nextMapping, finishedMappings)
      case None =>
        currentMapping match
          case Some(finished) => finishedMappings :+ finished
          case None           => finishedMappings

  loop(input, None, Vector.empty[InsaneMapping])

extension (i: BigInt) def isBetween(start: BigInt, end: BigInt): Boolean = start <= i && i <= end

case class InsaneRule(destinationStart: BigInt, sourceStart: BigInt, rangeLength: BigInt):
  override def toString: String = s"{($sourceStart-$sourceEnd) => $delta => ($destinationStart-$destinationEnd)}"
  val delta: BigInt             = destinationStart - sourceStart
  val destinationEnd: BigInt    = destinationStart + rangeLength - 1
  val sourceEnd: BigInt         = sourceStart + rangeLength - 1
  def getDestinationValue(sourceValue: BigInt): Option[BigInt] =
    if sourceValue.isBetween(sourceStart, sourceStart + rangeLength - 1) then Some(sourceValue + delta)
    else None
  val sourceRange: Range      = Range(sourceStart, sourceEnd)
  val destinationRange: Range = Range(destinationStart, destinationEnd)
  def toRule: Rule            = Rule(sourceRange, destinationRange, delta)

case class InsaneMapping(rules: Vector[InsaneRule]):
  def addRule(r: InsaneRule): InsaneMapping = this.copy(rules = rules :+ r)
  def mapValueToDestination(i: BigInt): BigInt =
    rules.flatMap(rule => rule.getDestinationValue(i)).headOption match
      case Some(mappedValue) => mappedValue
      case None              => i
  def toSaneMapping: Mapping = Mapping(rules.map(_.toRule))

case class Rule(source: Range, sink: Range, delta: BigInt):
  override def toString: String = s"{$source => $delta => $sink}"
  def applyToRange(range: Range): Option[Range] =
    if !range.ruleIsApplicable(this) then None
    else
      val start = if range.start < source.start then sink.start else range.start + delta
      val end   = if range.end > source.end then sink.end else range.end + delta
      Some(Range(start, end))

case class Mapping(rules: Vector[Rule])

case class Range(start: BigInt, end: BigInt):
  override def toString: String = s"($start-$end)"
  def ruleIsApplicable(rule: Rule): Boolean =
    !(rule.source.start > end || rule.source.end < start)
  private def remove(otherRange: Range): Vector[Range] =
    lazy val leftOverlap: Boolean = otherRange.start < start && otherRange.end > start && otherRange.end < end
    lazy val midOverlap: Boolean =
      otherRange.start >= start && otherRange.start <= end && otherRange.end >= start && otherRange.end <= end
    lazy val rightOverlap: Boolean = otherRange.start > start && otherRange.start < end && otherRange.end > end
    lazy val fullOverlap: Boolean  = otherRange.start <= start && otherRange.end >= end

    if leftOverlap then
      val rightRemaining = Range(otherRange.end + 1, end)
      Vector(rightRemaining)
    else if midOverlap then
      val leftRemaining  = Range(start, otherRange.start)
      val rightRemaining = Range(otherRange.end + 1, end)
      Vector(leftRemaining, rightRemaining)
    else if rightOverlap then
      val rightRemaining = Range(otherRange.end + 1, end)
      Vector(rightRemaining)
    else if fullOverlap then Vector.empty[Range]
    else Vector(this)

  def applyMapping(mapping: Mapping): Vector[Range] =
    val sortedApplicableRules =
      mapping.rules.filter(ruleIsApplicable).sortWith((r1, r2) => r1.source.start < r2.source.start)
    if sortedApplicableRules.isEmpty then Vector(this)
    else
      val mappedRanges        = sortedApplicableRules.flatMap(rule => rule.applyToRange(this))
      val untouchedRuleRanges = sortedApplicableRules.map(_.source)
      val removed = untouchedRuleRanges.foldLeft(Vector(this)) { case (remainingRanges, newRemovedRange) =>
        remainingRanges.flatMap(_.remove(newRemovedRange))
      }
      (removed ++ mappedRanges).sortWith(_.start < _.start)
