package com.jbullock.aoc2017.day25

import scala.io.Source

@main def solvePuzzle(): Unit =
  val start = State(Map.empty[Int, Int], 0, Letter.A)
  val end   = start.inNSteps(12425180)
  println(s"Part 1: ${end.checksum}")

enum Letter:
  case A, B, C, D, E, F

case class State(tape: Map[Int, Int], position: Int, state: Letter):
  def get(k: Int): Int           = tape.getOrElse(k, 0)
  def set(v: Int): Map[Int, Int] = tape.updated(position, v)
  val left: Int                  = position - 1
  val right: Int                 = position + 1
  val current: Int               = get(position)
  def zero: Map[Int, Int]        = set(0)
  def one: Map[Int, Int]         = set(1)
  def next: State =
    state match
      case Letter.A =>
        if current == 0 then State(one, right, Letter.B)
        else State(zero, right, Letter.F)
      case Letter.B =>
        if current == 0 then State(zero, left, Letter.B)
        else State(one, left, Letter.C)
      case Letter.C =>
        if current == 0 then State(one, left, Letter.D)
        else State(zero, right, Letter.C)
      case Letter.D =>
        if current == 0 then State(one, left, Letter.E)
        else State(one, right, Letter.A)
      case Letter.E =>
        if current == 0 then State(one, left, Letter.F)
        else State(zero, left, Letter.D)
      case Letter.F =>
        if current == 0 then State(one, right, Letter.A)
        else State(zero, left, Letter.E)
  def inNSteps(n: Int): State     = (1 to n).foldLeft(this)((s, _) => s.next)
  def checksum: Int               = tape.count((_, v) => v == 1)
