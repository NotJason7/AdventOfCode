package com.jbullock.aoc2017.day20

import annotation.tailrec
import io.Source
object Puzzle:
  @main def solve(): Unit =
    val input              = Source.fromResource("aoc/2017/Day20/Input.txt").getLines.toVector
    val particles          = input.map(Particle.fromString)
    val lowestAcceleration = particles.minBy(_.acceleration.absolute)
    println(s"Part 1: ${particles.indexOf(lowestAcceleration)}")

case class Position(x: Int, y: Int, z: Int):
  def update(v: Velocity): Position =
    Position(x + v.x, y + v.y, z + v.z)
case class Velocity(x: Int, y: Int, z: Int):
  def update(a: Acceleration): Velocity =
    Velocity(x + a.x, y + a.y, z + a.z)
case class Acceleration(x: Int, y: Int, z: Int):
  def absolute: Int = x.abs + y.abs + z.abs
case class Particle(position: Position, velocity: Velocity, acceleration: Acceleration):
  def update: Particle =
    val newVelocity = velocity.update(acceleration)
    val newPosition = position.update(newVelocity)
    Particle(newPosition, newVelocity, acceleration)
object Particle:
  def fromString(s: String): Particle =
    def extractValues(s: String): Vector[Int] = s.drop(3).dropRight(1).split(',').toVector.map(_.toInt)
    val Vector(pString, vString, aString)     = s.split(", ").toVector
    val Vector(px, py, pz)                    = extractValues(pString)
    val Vector(vx, vy, vz)                    = extractValues(vString)
    val Vector(ax, ay, az)                    = extractValues(aString)
    val position                              = Position(px, py, pz)
    val velocity                              = Velocity(vx, vy, vz)
    val acceleration                          = Acceleration(ax, ay, az)
    Particle(position, velocity, acceleration)
