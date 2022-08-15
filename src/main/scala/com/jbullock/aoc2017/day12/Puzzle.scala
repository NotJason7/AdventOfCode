package com.jbullock.aoc2017.day12

import scala.annotation.tailrec
import scala.io.Source

@main
def solvePuzzle(): Unit =
  println(s"Part 1: ${Puzzle.part1}")
  println(s"Part 2: ${Puzzle.part2}")

object Puzzle:
  val input: Vector[String] = Source.fromResource("aoc/2017/Day12/Input.txt")
    .getLines.toVector
  given pipes: Vector[Pipe] = input.map(Pipe.fromString)
  val networks: Set[Network] = pipes.map(Network.fromPipe).toSet
  def part1: Int = Network.fromPipe(pipes.filter(_.id == 0).head).pipes.size
  def part2: Int = networks.size

case class Network(pipes: Set[Pipe])
object Network:
  def fromPipe(p: Pipe)(using allPipes: Vector[Pipe]): Network =
    @tailrec
    def loop(networkPipes: Set[Pipe], toAdd: Set[Int]): Network =
      if toAdd.isEmpty then Network(networkPipes)
      else
        val pipe = allPipes.filter(_.id == toAdd.head).head
        val nextNetworkPipes = networkPipes ++ Set(pipe)
        val nextToAdd = toAdd.drop(1) ++ pipe.connections.filterNot(c => nextNetworkPipes.map(_.id).contains(c))
        loop(nextNetworkPipes, nextToAdd)
    loop(Set(p), p.connections)

case class Pipe(id: Int, connections: Set[Int])
object Pipe:
  def fromString(s: String): Pipe =
    val split = s.replace(" ","").split("<->").toVector
    val id = split.head.toInt
    val connections = split.tail.flatMap(_.toString.split(",").toVector.map(_.toInt)).toSet
    Pipe(id, connections)
