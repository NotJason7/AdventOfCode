package com.jbullock.aoc2021.day5

case class Vector(from: Coordinate, to: Coordinate) {
  val isVertical: Boolean = from.x == to.x
  val isHorizontal: Boolean = from.y == to.y
  val isLeftToRight: Boolean = from.x <= to.x
  val isBottomToTop: Boolean = from.y <= to.y
  override def toString: String = s"${from.x},${from.y} -> ${to.x},${to.y}"
}
