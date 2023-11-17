package com.jbullock.aoc2020.day20

import scala.annotation.tailrec

@main def solvePuzzle(): Unit =
  val input  = scala.io.Source.fromResource("aoc/2020/Day20/Input.txt").getLines.toVector
  val pieces = input.filter(_.nonEmpty).sliding(11, 11).map(Piece.fromInputVector).toVector
  val borderedPieces = pieces.map { piece =>
    val otherEdges = pieces.filterNot(_.id == piece.id).flatMap(_.possibleEdges).toSet
    piece.setBorder(otherEdges)
  }
  val part1 = borderedPieces.filter(_.borderedEdges == 2).map(piece => BigInt.apply(piece.id)).product
  println(s"Part 1: $part1")
  val part2 = Puzzle.fromPieces(borderedPieces).countNonSeaMonsterHash
  println(s"Part 2: $part2")

case class Puzzle(image: Vector[String]):
  private def rotate: Puzzle = Puzzle(image.indices.map(i => image.map(s => s(i)).mkString.reverse).toVector)
  private def flip: Puzzle   = Puzzle(image.map(_.reverse))
  private def allOrientations: Vector[Puzzle] = Vector(
    this,
    this.rotate,
    this.rotate.rotate,
    this.rotate.rotate.rotate,
    this.flip,
    this.flip.rotate,
    this.flip.rotate.rotate,
    this.flip.rotate.rotate.rotate
  )

  def countNonSeaMonsterHash: Int =
    image.reduce(_ + _).count(_ == '#') - (15 * allOrientations.map(_.seaMonsterCount).max)

  private def seaMonsterCount: Int =
    val seaMonsterRegex = "..................#.#....##....##....###.#..#..#..#..#..#..."
    image
      .sliding(3)
      .flatMap(lines => lines.flatMap(_.sliding(20)).transpose.map(_.mkString))
      .count(s => s.matches(seaMonsterRegex))

object Puzzle:
  def fromPieces(pieces: Vector[Piece]): Puzzle =

    def findStart(pieces: Vector[Piece]): Piece =
      val startingCorner = pieces.filter(_.borderedEdges == 2).head
      val cornerOutline  = Outline.fromDirectionBorder(Direction.Up).setAsBorder(Direction.Left)
      startingCorner.alignTo(cornerOutline).get

    @tailrec def buildRows(
        currentRow: Vector[Piece],
        remainingPieces: Vector[Piece],
        allRows: Vector[Vector[Piece]]
    ): Vector[Vector[Piece]] =
      if remainingPieces.isEmpty then allRows.appended(currentRow)
      else if currentRow.isEmpty then
        allRows.lastOption match
          case None =>
            val start         = findStart(remainingPieces)
            val nextRemaining = remainingPieces.filterNot(_.id == start.id)
            buildRows(currentRow :+ start, nextRemaining, allRows)
          case Some(lastRow) =>
            val outline = Outline.fromDirectionString(Direction.Up, lastRow.head.down).setAsBorder(Direction.Left)
            val matchingPieces = remainingPieces.flatMap(piece => piece.alignTo(outline))
            val rowStart       = matchingPieces.head
            val nextRemaining  = remainingPieces.filterNot(_.id == rowStart.id)
            buildRows(currentRow :+ rowStart, nextRemaining, allRows)
      else
        val current = currentRow.last
        if current.right.contains("X") then
          buildRows(Vector.empty[Piece], remainingPieces, allRows.appended(currentRow))
        else
          val index       = currentRow.indexOf(current) + 1
          val nextPiece   = remainingPieces.find(_.possibleNonBorderEdges.contains(current.right)).get
          val baseOutline = Outline.fromDirectionString(Direction.Left, current.right)
          val nextOutline = allRows.lastOption match
            case Some(above) =>
              baseOutline.set(Direction.Up, above(index).down)
            case None => baseOutline.setAsBorder(Direction.Up)

          val orientedNextPiece   = nextPiece.alignTo(nextOutline).get
          val nextRow             = currentRow :+ orientedNextPiece
          val nextRemainingPieces = remainingPieces.filterNot(_.id == nextPiece.id)
          buildRows(nextRow, nextRemainingPieces, allRows)
    val puzzlePieces = buildRows(Vector.empty[Piece], pieces, Vector.empty[Vector[Piece]])
    Puzzle(puzzlePieces.flatMap(pieceRow => pieceRow.map(_.image).transpose.map(_.reduce(_ + _))))

enum Direction:
  case Up, Down, Left, Right

case class Edge(field: Option[String], isBorder: Boolean = false):
  def matches(s: String): Boolean = (field, isBorder) match
    case (Some(matchingField), false) => s == matchingField
    case (_, true)                    => s.contains("X")
    case (None, false)                => true

case class Outline(up: Edge, down: Edge, left: Edge, right: Edge):
  def set(d: Direction, s: String): Outline = d match
    case Direction.Up    => this.copy(up = Edge(Some(s)))
    case Direction.Down  => this.copy(down = Edge(Some(s)))
    case Direction.Left  => this.copy(left = Edge(Some(s)))
    case Direction.Right => this.copy(right = Edge(Some(s)))
  def setAsBorder(d: Direction): Outline = d match
    case Direction.Up    => this.copy(up = Edge(None, true))
    case Direction.Down  => this.copy(down = Edge(None, true))
    case Direction.Left  => this.copy(left = Edge(None, true))
    case Direction.Right => this.copy(right = Edge(None, true))
  def matches(piece: Piece): Boolean =
    up.matches(piece.up) && down.matches(piece.down) && left.matches(piece.left.reverse) && right.matches(piece.right)
object Outline:
  private val BaseOutline: Outline                          = Outline(Edge(None), Edge(None), Edge(None), Edge(None))
  def fromDirectionBorder(d: Direction): Outline            = BaseOutline.setAsBorder(d)
  def fromDirectionString(d: Direction, s: String): Outline = BaseOutline.set(d, s)

case class Piece(id: Int, pixels: Vector[String]):
  def up: String                           = pixels.head
  def down: String                         = pixels.last
  def left: String                         = pixels.map(_.head).mkString.reverse
  def right: String                        = pixels.map(_.last).mkString
  private def currentEdges: Vector[String] = Vector(up, down, left, right)
  def possibleEdges: Set[String]           = currentEdges.toSet ++ currentEdges.map(_.reverse)
  def possibleNonBorderEdges: Set[String]  = possibleEdges - (1 to up.length).map(_ => "X").mkString
  def setBorder(otherEdges: Set[String]): Piece =
    @tailrec def loop(rotatedPiece: Piece, updatedPiece: Piece, rotationsLeft: Int): Piece =
      if rotationsLeft == 0 then updatedPiece
      else
        val isABorder = !otherEdges.contains(rotatedPiece.up)
        val newUpdatedPiece =
          if isABorder then
            val border = updatedPiece.pixels.head.head + (2 until updatedPiece.pixels.length)
              .map(_ => "X")
              .mkString + updatedPiece.pixels.head.last
            updatedPiece.copy(pixels = border +: updatedPiece.pixels.drop(1))
          else updatedPiece
        loop(rotatedPiece.rotate, newUpdatedPiece.rotate, rotationsLeft - 1)
    loop(this, this, 4)

  private def orientations: Vector[Piece] = Vector(
    this,
    this.rotate,
    this.rotate.rotate,
    this.rotate.rotate.rotate,
    this.flip,
    this.flip.rotate,
    this.flip.rotate.rotate,
    this.flip.rotate.rotate.rotate
  )
  def alignTo(outline: Outline): Option[Piece] = orientations.find(outline.matches)
  def borderedEdges: Int                       = currentEdges.count(s => s.contains("X"))
  val image: Vector[String]                    = pixels.drop(1).dropRight(1).map(s => s.drop(1).dropRight(1))
  def rotate: Piece =
    val rotatedPixels = pixels.indices.map(index => pixels.map(s => s(index)).mkString.reverse).toVector
    this.copy(pixels = rotatedPixels)
  private def flip: Piece = this.copy(pixels = pixels.map(_.reverse))

object Piece:
  def fromInputVector(v: Vector[String]): Piece =
    val id = v.head match
      case s"Tile $id:" => id.toInt
    val pixels = v.tail
    Piece(id, pixels)
