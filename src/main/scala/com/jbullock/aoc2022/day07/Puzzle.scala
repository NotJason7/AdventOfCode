package com.jbullock.aoc2022.day07

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input          = io.Source.fromResource("aoc/2022/Day07/Input.txt").getLines.toVector
  val paths          = buildFileStructure(input)
  val directorySizes = paths.values.filter(_.isInstanceOf[Directory]).map(_.diskSize(paths))
  val part1          = directorySizes.filter(_ < 100000).sum
  println(s"Part 1: $part1")
  val freeSpace = 70000000 - paths.getOrElse(Directory.root.fullPath, Directory.root).diskSize(paths)
  val part2     = directorySizes.filter(_ + freeSpace > 30000000).min
  println(s"Part 2: $part2")

sealed trait Path:
  val path: String
  val name: String
  val fullPath: String = s"$path/$name"
  def diskSize(paths: Map[String, Path]): BigInt
object Path:
  def fromConsoleString(path: String, s: String): Path = s match
    case dir if dir.startsWith("dir ") => Directory.empty(path, dir.drop(4))
    case file =>
      val Vector(size, name) = file.split(" ").toVector
      File(path, name, BigInt(size.toInt))

case class Directory(path: String, name: String, contents: Vector[String]) extends Path:
  override def diskSize(paths: Map[String, Path]): BigInt = contents.flatMap(paths.get).map(_.diskSize(paths)).sum
  def register(newContents: Vector[String]): Directory    = this.copy(contents = contents ++ newContents)
object Directory:
  def empty(path: String, name: String): Directory = Directory(path, name, Vector.empty[String])
  val root: Directory                              = Directory.empty("", "root")
case class File(path: String, name: String, bytes: BigInt) extends Path:
  override def diskSize(paths: Map[String, Path]): BigInt = bytes

def buildFileStructure(console: Vector[String]): Map[String, Path] =

  @tailrec def process(console: Vector[String], dir: Directory, paths: Map[String, Path]): Map[String, Path] =
    console.headOption match
      case None => paths
      case Some("$ ls") =>
        val contents = console.tail.takeWhile(o => !o.startsWith("$")).map(s => Path.fromConsoleString(dir.fullPath, s))
        val contentStrings = contents.map(_.fullPath)
        val files          = contents.filter(_.isInstanceOf[File])
        val nextOutput     = console.tail.drop(contents.size)
        val nextDir        = dir.register(contentStrings)
        val nextPaths = files.foldLeft(paths.updated(nextDir.fullPath, nextDir))((p, f) => p.updated(f.fullPath, f))
        process(nextOutput, nextDir, nextPaths)
      case Some("$ cd /") => process(console.tail, Directory.root, paths)
      case Some("$ cd ..") =>
        paths.get(dir.path) match
          case Some(dir: Directory) => process(console.tail, dir, paths)
          case _                    => throw RuntimeException(s"Could not go up a level from $dir")
      case Some(cd) if cd.startsWith("$ cd ") =>
        val nextDirectory = Directory.empty(dir.fullPath, cd.drop(5))
        process(console.tail, nextDirectory, paths)
      case _ => throw RuntimeException(s"Unable to process console message ${console.head}")

  process(console, Directory.empty("", ""), Map.empty[String, Path])
