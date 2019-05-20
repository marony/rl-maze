package com.binbo_kodakusan

sealed abstract class Direction
object Direction {
  final case object None extends Direction
  final case object West extends Direction
  final case object North extends Direction
  final case object East extends Direction
  final case object South extends Direction

  def fromInt(d: Int): Direction = {
    d match {
      case 0 => Direction.West
      case 1 => Direction.North
      case 2 => Direction.East
      case 3 => Direction.South
      case _ => Direction.None
    }
  }

  def toInt(d: Direction): Int = {
    d match {
      case Direction.West => 0
      case Direction.North => 1
      case Direction.East => 2
      case Direction.South => 3
      case _ => -1
    }
  }
}
