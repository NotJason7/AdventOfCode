package com.jbullock.aoc2024.day09

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input      = scala.io.Source.fromResource("aoc/2024/Day09/Sample.txt").mkString
  val fileBlocks = FileBlock.blocksFromString(input)
  println(fileBlocks)

//  val diskMap = DiskMap(input)
//  val visualBlocks = diskMap.visualBlocks
//  val fileIDs      = diskMap.fileIDs
//  val fileIDsString = fileIDs.map {
//    case Some(i) => i.toInt
//    case None    => '.'
//  }.mkString
//  val compressed = diskMap.compress
//  val compressedString = compressed.map {
//    case Some(i) => i.toInt
//    case None    => '.'
//  }.mkString
//  println(visualBlocks)
//  println(fileIDsString)
//  println(diskMap.empties)
//  println(compressedString)
//  println(visualBlocks)
//  val fileIDLocations = diskMap.fileIDs
//  println(fileIDLocations)
//  val part1 = diskMap.checksum
//  println(s"Part 1: $part1")
//
case class FileBlock(size: BigInt, id: Int, isEmpty: Boolean)
object FileBlock:
  def blocksFromString(s: String): Seq[FileBlock] =
    s.toSeq.map(_.asDigit).zipWithIndex.map { case (size, index) => FileBlock(size, index, index % 2 == 0) }

case class DiskMap(stringMap: String):
  val fileIDs: Seq[Option[BigInt]] =
    stringMap
      .map(_.asDigit)
      .zipWithIndex
      .foldLeft(Seq.empty[Option[BigInt]], 0) { case ((currentIDs, currentIndex), (repeats, index)) =>
        val isFileIndex: Boolean = index % 2 == 0
        val nextIDs = currentIDs ++ (0 until repeats).map(_ => if isFileIndex then Some(BigInt(currentIndex)) else None)
        val nextIndex = if isFileIndex then currentIndex + 1 else currentIndex
        (nextIDs, nextIndex)
      }
      ._1

  val visualBlocks: String = stringMap
    .map(_.asDigit)
    .zipWithIndex
    .foldLeft(("", 0)) { case ((string, nextIndex), (repeats, index)) =>
      val isFileIndex: Boolean = (index % 2 == 0)
      if isFileIndex then (string ++ nextIndex.toString.repeat(repeats), nextIndex + 1)
      else (string ++ ".".repeat(repeats), nextIndex)
    }
    ._1
  val blockIndexes: Seq[(Option[BigInt], Int)]       = fileIDs.zipWithIndex
  def empties: Seq[(Option[BigInt], Int)]            = blockIndexes.filter(_._1.isEmpty)
  def fileIdsInMoveOrder: Seq[(Option[BigInt], Int)] = blockIndexes.filter(_._1.isDefined).reverse

  def compress: Seq[Option[BigInt]] =
    empties
      .zip(fileIdsInMoveOrder)
      .filter { case ((_, emptyIndex), (_, replacementIndex)) =>
        emptyIndex < replacementIndex
      }
      .flatMap { case ((empty, emptyIndex), (replacement, replacementIndex)) =>
        Seq((replacement, emptyIndex), (empty, replacementIndex))
      }
      .foldLeft(fileIDs) { case (s, (c, i)) => s.updated(i, c) }

  def checksum: BigInt = compress.zipWithIndex.map { case (x, y) =>
    x match
      case None    => BigInt(0)
      case Some(n) => n * BigInt(y)
  }.sum
