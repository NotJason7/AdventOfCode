package com.jbullock.aoc2023.day10

import scala.annotation.tailrec

case class Position(x: Int, y: Int):
  def move(direction: Direction): Position = Position(x + direction.dx, y + direction.dy)
  def adjacent: Vector[Position]           = Vector(Up, Down, Left, Right).map(this.move)

trait Direction(val dx: Int, val dy: Int, val opposite: Direction)
case object Up    extends Direction(0, -1, Down)
case object Down  extends Direction(0, 1, Up)
case object Left  extends Direction(-1, 0, Right)
case object Right extends Direction(1, 0, Left)

enum Tile(val directions: Set[Direction], val name: String):
  case Vertical extends Tile(Set(Up, Down), "|")
  case Horizontal extends Tile(Set(Left, Right), "-")
  case TopLeft extends Tile(Set(Down, Right), "F")
  case TopRight extends Tile(Set(Down, Left), "7")
  case BottomLeft extends Tile(Set(Up, Right), "L")
  case BottomRight extends Tile(Set(Up, Left), "J")
  case Start extends Tile(Set(Up, Down, Left, Right), "S")
  case Ground extends Tile(Set.empty[Direction], ".")
  case ExtraGround extends Tile(Set.empty[Direction], " ")
object Tile:
  def fromChar(c: Char): Tile = c match
    case '|' => Vertical
    case '-' => Horizontal
    case 'F' => TopLeft
    case '7' => TopRight
    case 'L' => BottomLeft
    case 'J' => BottomRight
    case 'S' => Start
    case '.' => Ground
    case ' ' => ExtraGround

case class Step(tile: Tile, position: Position):
  def toMap: Map[Position, Tile] = Map(position -> tile)

extension (tileMap: Map[Position, Tile])
  def xMin: Int = tileMap.keys.map(_.x).min
  def xMax: Int = tileMap.keys.map(_.x).max
  def yMin: Int = tileMap.keys.map(_.y).min
  def yMax: Int = tileMap.keys.map(_.y).max
  def findLoop: Vector[Step] =
    val start = tileMap.find((_, v) => v == Tile.Start) match
      case Some((position, tile)) => Step(tile, position)
      case None                   => throw RuntimeException("No start tile!")

    @tailrec def loop(current: Step, traversed: Vector[Step]): Vector[Step] =
      val validNextDirectionPositions = current.tile.directions
        .map(d => (d, current.position.move(d)))
        .filter((direction, position) => tileMap.contains(position))
      validNextDirectionPositions
        .map((direction, position) => (direction, Step(tileMap(position), position)))
        .filterNot((direction, step) => traversed.contains(step))
        .find((direction, step) => step.tile.directions.map(_.opposite).contains(direction)) match
        case Some((direction, validNextStep)) => loop(validNextStep, traversed :+ validNextStep)
        case None                             => traversed

    loop(start, Vector.empty[Step])

  def replaceNonLoopPipes(): Map[Position, Tile] =
    val loopMap = tileMap.findLoop.flatMap(_.toMap).toMap
    tileMap.map {
      case (position, tile) if loopMap.contains(position)                         => (position, tile)
      case (position, tile) if !Set(Tile.Ground, Tile.ExtraGround).contains(tile) => (position, Tile.Ground)
      case (position, tile)                                                       => (position, tile)
    }

  def toStrings: Vector[String] =
    (yMin to yMax).toVector.map(y => (xMin to xMax).toVector.map(x => tileMap(Position(x, y)).name).mkString)

  def draw(): Unit = toStrings.foreach(println)

  def countEnclosedTiles: Int =
    val loopOnlyTiles    = tileMap.replaceNonLoopPipes()
    val totalGroundTiles = loopOnlyTiles.count((_, tile) => tile == Tile.Ground)
    val outerBoundGroundTiles =
      tileMap
        .filter((position, tile) =>
          (Set(xMin, xMax).contains(position.x) || Set(yMin, yMax).contains(position.y)) && tile == Tile.Ground
        )
        .keys
        .toVector
    val outerGroundTiles = countGroundTilesWithoutCrossingPipes(outerBoundGroundTiles)
    println(
      s"Found $outerGroundTiles outer ground tiles, with $totalGroundTiles total ground tiles, leaving ${totalGroundTiles - outerGroundTiles} inner ground tiles"
    )
    totalGroundTiles - outerGroundTiles

  def countGroundTilesWithoutCrossingPipes(startPositions: Vector[Position]): Int =
    @tailrec def loop(toSearch: Vector[Position], found: Vector[Position]): Int =
      toSearch.headOption match
        case Some(next) =>
          val nextFound    = found :+ next
          val movableTiles = Vector(Tile.Ground, Tile.ExtraGround)
          val nextToSearch = toSearch.tail ++ next.adjacent
            .filterNot((toSearch ++ nextFound).contains)
            .filter(position => movableTiles.contains(tileMap.getOrElse(position, Tile.Vertical)))
          loop(nextToSearch, nextFound)
        case None => found.count(position => tileMap.getOrElse(position, Tile.ExtraGround) == Tile.Ground)
    loop(startPositions.take(1), Vector.empty[Position])

  def expand(): Map[Position, Tile] =
    val expandedTileStrings = toStrings
      .flatMap(row => Vector(row, " " * row.length))
      .dropRight(1)
      .map(_.toVector.flatMap(c => Vector(c, ' ')).dropRight(1).mkString)
    val expanded = (for
      y <- expandedTileStrings.indices
      x <- expandedTileStrings.head.indices
    yield Position(x, y) -> expandedTileStrings(y)(x)).toMap
    (for
      y <- expandedTileStrings.indices
      x <- expandedTileStrings.head.indices
      position = Position(x, y)
      up       = expanded.getOrElse(position.move(Up), 'X')
      down     = expanded.getOrElse(position.move(Down), 'X')
      left     = expanded.getOrElse(position.move(Left), 'X')
      right    = expanded.getOrElse(position.move(Right), 'X')
      c =
        if expanded(position) != ' ' then expanded(position)
        else
          (up, down, left, right) match
            case (u, d, _, _) if "F|7S".contains(u) && "L|JS".contains(d) => '|'
            case (u, _, l, _) if "F|7S".contains(u) && "F-LS".contains(l) => 'J'
            case (u, _, _, r) if "F|7S".contains(u) && "7-JS".contains(r) => 'L'
            case (_, d, l, _) if "L|JS".contains(d) && "F-LS".contains(l) => '7'
            case (_, d, _, r) if "L|JS".contains(d) && "7-JS".contains(r) => 'F'
            case (_, _, l, r) if "F-LS".contains(l) && "7-JS".contains(r) => '-'
            case _                                                        => ' '
    yield Position(x, y) -> Tile.fromChar(c)).toMap

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2023/Day10/Input.txt").getLines.toVector
  val tileMap = (for
    y <- input.indices
    x <- input.head.indices
  yield Position(x, y) -> Tile.fromChar(input(y)(x))).toMap
  val loop  = findLoop(tileMap)
  val part1 = loop.length / 2
  println(s"Part 1: $part1")
  val expandedTileMap = tileMap.replaceNonLoopPipes().expand()
//  expandedTileMap.draw()
  val part2 = expandedTileMap.countEnclosedTiles
  println(s"Part 2: $part2")
