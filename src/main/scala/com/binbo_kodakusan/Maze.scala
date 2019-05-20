package com.binbo_kodakusan

import sun.security.jca.GetInstance

import scala.util.Random

/**
  * 迷路の1マス
  *
  * どちらの方向に壁があるか
  *
  * @param west
  * @param north
  * @param east
  * @param south
  */
case class MazePiece(west: Boolean, north: Boolean, east: Boolean, south: Boolean) {
  def isWall(dir: Direction): Boolean = {
    dir match {
      case Direction.West => west
      case Direction.North => north
      case Direction.East => east
      case Direction.South => south
      case _ => false
    }
  }
}

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
  val Start = (0, 0)
  val Goal = (width - 1, height - 1)

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

  private[this] def getWallIndex(x: Int, y: Int): Int = {
    (height + 1) * y + x
  }

  def getPiece(x: Int, y: Int): MazePiece = maze(getWallIndex(x, y))
  private[this] def setPiece(x: Int, y: Int, p: MazePiece): Unit = maze(getWallIndex(x, y)) = p

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
    * その方向が迷路内かどうか
    *
    * @param x
    * @param y
    * @param dir
    * @return
    */
  private[this] def movableDelata(x: Int, y: Int, dir: Direction): (Int, Int, Direction) = {
    dir match {
      case Direction.West if x <= 0 => (0, 0, Direction.None)
      case Direction.West => (-1, 0, Direction.West)
      case Direction.North if y <= 0 => (0, 0, Direction.None)
      case Direction.North => (0, -1, Direction.North)
      case Direction.East if x >= width - 1 => (0, 0, Direction.None)
      case Direction.East => (1, 0, Direction.East)
      case Direction.South if y >= height - 1 => (0, 0, Direction.None)
      case Direction.South => (0, 1, Direction.South)
      case _ => ???
    }
  }

  def movable(x: Int, y: Int, dir: Direction): (Int, Int, Direction) = {
    val (dx, dy, dir2) = movableDelata(x, y, dir)
    if (dir2 == Direction.None) {
      // 移動できない(迷路の端)
      (0, 0, dir2)
    } else {
      // 壁をチェックする
      val p = getPiece(x, y)
      // 移動可能か
      if (p.isWall(dir)) {
        // 移動できない(壁)
        (0, 0, Direction.None)
      } else {
        // 移動できた！！
        (x + dx, y + dy, dir2)
      }
    }
  }

  /**
    * クラスタリングによる迷路作成
    * @param x
    * @param y
    * @param cluster
    */
  private[this] def making(x: Int, y: Int, cluster: Array[Int]): Unit = {
    def getIndex(x: Int, y: Int): Int = {
      height * y + x
    }

    val c1 = cluster(getIndex(x, y))
    var finish = false
    while (!finish) {
      // 方向をランダムに決める
      val rd = rand.nextInt(4)
      val (dx, dy, dir) = movableDelata(x, y, Direction.fromInt(rd))
      if (dir != Direction.None) {
        // 進めるならば
        val x2 = x + dx
        val y2 = y + dy
        val c2 = cluster(getIndex(x2, y2))
        if (c1 != c2) {
          // クラスタが違うならば壁を壊す
          setWall(x, y, dir, false)
          // 同じクラスタに塗り替える
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
    val p1 = getPiece(x, y)
    dir match {
      case Direction.West =>
        // 左の壁
        setPiece(x, y, MazePiece(wall, p1.north, p1.east, p1.south))
        if (x > 0) {
          // 一番左じゃないのでひとつ左のマスの右の壁も更新
          val p2 = getPiece(x - 1, y)
          setPiece(x - 1, y, MazePiece(p2.west, p2.north, wall, p2.south))
        }
      case Direction.North =>
        // 上の壁
        setPiece(x, y, MazePiece(p1.west, wall, p1.east, p1.south))
        if (y > 0) {
          // 一番上じゃないのでひとつ上のマスの下の壁も更新
          val p2 = getPiece(x, y - 1)
          setPiece(x, y - 1, MazePiece(p2.west, p2.north, p2.east, wall))
        }
      case Direction.East =>
        // 右の壁
        setPiece(x, y, MazePiece(p1.west, p1.north, wall, p1.south))
        if (x < width) {
          // 一番右じゃないのでひとつ右のマスの左の壁も更新
          val p2 = getPiece(x + 1, y)
          setPiece(x + 1, y, MazePiece(wall, p2.north, p2.east, p2.south))
        }
      case Direction.South =>
        // 下の壁
        setPiece(x, y, MazePiece(p1.west, p1.north, p1.east, wall))
        if (y < height) {
          // 一番下じゃないのでひとつ下のマスの上の壁も更新
          val p2 = getPiece(x, y + 1)
          setPiece(x, y + 1, MazePiece(p2.west, wall, p2.east, p1.south))
        }
      case _ => ???
    }
  }
}
