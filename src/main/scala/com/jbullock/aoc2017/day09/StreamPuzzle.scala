package com.jbullock.aoc2017.day09

import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.{Stream, text}
import fs2.io.file.{Files, Path}

object StreamPuzzle extends IOApp.Simple:
  val state: Stream[IO, StreamState] = Files[IO].readAll(Path("src/main/resources/aoc/2017/Day09/Input.txt"))
    .through(text.utf8.decode).flatMap(s => Stream.emits(s.toVector))
    .fold(StreamState(false, false, 0, 0, 0))((state, char) => state.next(char))

  def run: IO[Unit] = state.evalTap(s => IO.println(s"Part 1: ${s.score}\nPart 2: ${s.removed}")).compile.drain

case class StreamState(inGarbage: Boolean, skip: Boolean, level: Int, score: Int, removed: Int):
  def next(c: Char): StreamState = c match
      case _ if skip => this.copy(skip = false)
      case '{' if !inGarbage => this.copy(level = level + 1)
      case '}' if !inGarbage => this.copy(level = level - 1, score = score + level)
      case '<' if !inGarbage => this.copy(inGarbage = true)
      case _ if !inGarbage   => this
      case '!' if inGarbage => this.copy(skip = true)
      case '>' if inGarbage => this.copy(inGarbage = false)
      case _ if inGarbage   => this.copy(removed = removed + 1)
