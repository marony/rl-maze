package com.binbo_kodakusan

import sun.security.jca.GetInstance

import scala.util.Random

sealed abstract class Direction
object Direction {
  final case object None extends Direction
  final case object West extends Direction
  final case object North extends Direction
  final case object East extends Direction
  final case object South extends Direction
}

case class MazePiece(west: Boolean, north: Boolean, east: Boolean, south: Boolean)

/**
  * 迷路の状態
  * 基本的にそのマスの左と上の壁の状態を持つが、
  * (width + 1, y)のマスは一番右のマスの右の壁の状態を持ち
  * (x, height + 1)のマスは一番下のマスの上の壁の状態を持つ
  *
  * 利便性のため、右と下の壁の状態も隣の左と上の壁の状態に連動して更新する
  *
  * 例)
  * 3x3の迷路の場合
  * (0, 0)は(0, 0)の左と上の壁の状態を持つ
  * (3, 0)は(2, 0)の左の壁の状態を持つ
  * (0, 3)は(0, 2)の上の壁の状態を持つ
  *
  * @param width
  * @param height
  */
case class Maze(width: Int, height: Int) {
  // 全てを壁で埋める
  val maze = Array.fill((width + 1) * (height + 1))(MazePiece(true, true, true, true))
  // 右端は左側の壁のみ
  for (y <- 0 until height) {
    maze(getWallIndex(width, y)) = MazePiece(true, false, false, false)
  }
  // 下端は上側の壁のみ
  for (x <- 0 until width) {
    maze(getWallIndex(x, height)) = MazePiece(false, true, false, false)
  }

  val rand = new Random

  def getWallIndex(x: Int, y: Int): Int = {
    (height + 1) * y + x
  }
  def getIndex(x: Int, y: Int): Int = {
    height * y + x
  }

  /**
    * 迷路を作成する
    */
  def construct(): Unit = {
    val cluster = (1 to width * height).toArray
    while (cluster.exists(n => n != cluster(0))) {
      val x = rand.nextInt(width)
      val y = rand.nextInt(height)
      making(x, y, cluster)
    }
  }

  /**
    * 迷路を描画する
    */
  def draw(): Unit = {

  }

  private[this] def making(x: Int, y: Int, cluster: Array[Int]): Unit = {
    def func(rd: Int): (Int, Int, Direction) = {
      rd match {
        case 0 if x <= 0 => (0, 0, Direction.None)
        case 0 => (-1, 0, Direction.West)
        case 1 if y <= 0 => (0, 0, Direction.None)
        case 1 => (0, -1, Direction.North)
        case 2 if x >= width - 1 => (0, 0, Direction.None)
        case 2 => (1, 0, Direction.East)
        case 3 if y >= height - 1 => (0, 0, Direction.None)
        case 3 => (0, 1, Direction.South)
        case _ => ???
      }
    }

    val c1 = cluster(getIndex(x, y))
    var finish = false
    while (!finish) {
      val rd = rand.nextInt(4)
      val (dx, dy, dir) = func(rd)
      if (dir != Direction.None) {
        val x2 = x + dx
        val y2 = y + dy
        val c2 = cluster(getIndex(x2, y2))
        if (c1 != c2) {
          setWall(x, y, dir, false)
          // x ,yのクラスタに塗り替える
          for (i <- cluster.indices) {
            if (cluster(i) == c2) {
              cluster(i) = c1
            }
          }
        }
        finish = true
      }
    }
  }

  private def setWall(x: Int, y: Int, dir: Direction, wall: Boolean): Unit = {
    val p1 = maze(getWallIndex(x, y))
    dir match {
      case Direction.West =>
        // 左の壁
        maze(getWallIndex(x, y)) = MazePiece(wall, p1.north, p1.east, p1.south)
        if (x > 0) {
          // 一番左じゃないのでひとつ左のマスの右の壁も更新
          val p2 = maze(getWallIndex(x - 1, y))
          maze(getWallIndex(x - 1, y)) = MazePiece(p2.west, p2.north, wall, p2.south)
        }
      case Direction.North =>
        // 上の壁
        maze(getWallIndex(x, y)) = MazePiece(p1.west, wall, p1.east, p1.south)
        if (y > 0) {
          // 一番上じゃないのでひとつ上のマスの下の壁も更新
          val p2 = maze(getWallIndex(x, y - 1))
          maze(getWallIndex(x, y - 1)) = MazePiece(p2.west, p2.north, p2.east, wall)
        }
      case Direction.East =>
        // 右の壁
        maze(getWallIndex(x, y)) = MazePiece(p1.west, p1.north, wall, p1.south)
        if (x < width) {
          // 一番右じゃないのでひとつ右のマスの左の壁も更新
          val p2 = maze(getWallIndex(x + 1, y))
          maze(getWallIndex(x + 1, y)) = MazePiece(wall, p2.north, p2.east, p2.south)
        }
      case Direction.South =>
        // 下の壁
        maze(getWallIndex(x, y)) = MazePiece(p1.west, p1.north, p1.east, wall)
        if (y < height) {
          // 一番下じゃないのでひとつ下のマスの上の壁も更新
          val p2 = maze(getWallIndex(x, y + 1))
          maze(getWallIndex(x, y + 1)) = MazePiece(p2.west, wall, p2.east, p1.south)
        }
      case _ => ???
    }
  }
}
