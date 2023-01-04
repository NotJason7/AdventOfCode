package com.jbullock.aoc2022.day15

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2022/Day15/Input.txt").getLines.toVector
  val sensorBeacons = input.map("-?[0-9]+".r.findAllIn(_).map(_.toInt).toVector).map { case Vector(x1, y1, x2, y2) =>
    (Position(x1, y1), Position(x2, y2))
  }
  val sensors = sensorBeacons.map(_._1).toSet
  val beacons = sensorBeacons.map(_._2).toSet
  val y       = 2000000
  val sensorBeaconsInRange = sensorBeacons.filter { case (sensor, beacon) =>
    val distance = sensor.distanceFrom(beacon)
    (sensor.y - distance to sensor.y + distance).contains(y)
  }
  val part1 = sensorBeaconsInRange.flatMap { case (sensor, beacon) => sensor.safePlacesAroundY(beacon, y) }.toSet.size
  println(s"Part 1: $part1")
  val max = 4000000
  val part2 = (for
    y <- 0 to max
    x <- 0 to max
    beacon         = Position(x, y)
    closestSensor         = sensors.minBy(beacon.distanceFrom)
    existingBeacon = sensorBeacons.filter(_._1 == sensor).head._2
    if existingBeacon.distanceFrom(sensor) < beacon.distanceFrom(sensor)
  yield beacon).head.tuningFrequency
  println(s"Part 2: $part2")

case class Position(x: Int, y: Int):
  val tuningFrequency: Int           = x * 4000000 + y
  def distanceFrom(p: Position): Int = (p.x - x).abs + (p.y - y).abs
  def safePlacesAroundY(p: Position, yOfInterest: Int): Set[Position] =
    val distance = distanceFrom(p)
    (for
      dy <- -distance to distance
      if y + dy == yOfInterest
      dx <- -distance to distance
      if dx.abs + dy.abs <= distance
      if !Set(this, p).contains(Position(x + dx, y + dy))
    yield Position(x + dx, y + dy)).toSet
  override def toString = s"($x,$y)"
