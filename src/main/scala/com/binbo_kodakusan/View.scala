package com.binbo_kodakusan

import java.awt.BasicStroke
import java.awt.{ Color => AWTColor }
import scala.swing.{Dimension, Graphics2D}

import Const._

object View {
  private def drawBoard(g: Graphics2D, maze: Maze, ql: QL): Unit = {
    // 背景
    g.setColor(AWTColor.BLACK)
    g.fillRect(0, 0, maze.width * PieceSize, maze.height * PieceSize)

    // Q値の最大と最小を取って、0〜255に慣らす
    val max = ql.qvalue.map(_.max).max
    val min = ql.qvalue.map(_.min).min
    // 盤面
    for (y <- 0 until MazeWidth) {
      for (x <- 0 until MazeHeight) {

        val x1 = x * PieceSize
        val y1 = y * PieceSize
        val x2 = (x + 1) * PieceSize
        val y2 = (y + 1) * PieceSize

        // Q値で塗り潰し
        // 0〜225にする
        val c = ((ql.getMaxQValue(x, y) - min) * 256 / max).toInt
        val color = new AWTColor(c / 2, 0, 0)
        g.setColor(color)
        g.fillRect(x1 + 6, y1 + 6, PieceSize - 10, PieceSize - 10)
      }
    }

    // 線
    g.setColor(AWTColor.WHITE)
    g.setStroke(new BasicStroke(1f))
    for (y <- 0 until MazeWidth + 1) {
      for (x <- 0 until MazeHeight + 1) {

        val x1 = x * PieceSize
        val y1 = y * PieceSize
        val x2 = (x + 1) * PieceSize
        val y2 = (y + 1) * PieceSize

        val p = maze.maze((MazeHeight + 1) * y + x)
        if (p.west) {
          g.drawLine(x1, y1, x1, y2)
        }
        if (p.north) {
          g.drawLine(x1, y1, x2, y1)
        }
      }
    }
  }

  private def drawGoal(g: Graphics2D, maze: Maze): Unit = {
    // スタートとゴール
    {
      val x = maze.Start._1
      val y = maze.Start._2
      g.setColor(AWTColor.BLUE)
      g.fillOval(
        x * PieceSize,
        y * PieceSize,
        PieceSize, PieceSize)
    }
    {

      val x = maze.Goal._1
      val y = maze.Goal._2
      g.setColor(AWTColor.YELLOW)
      g.fillOval(
        x * PieceSize,
        y * PieceSize,
        PieceSize, PieceSize)
    }
  }

  private def drawPlayer(g: Graphics2D, maze: Maze, player: Player): Unit = {
    // プレイヤー
    {
      val x = player.px
      val y = player.py
      g.setColor(AWTColor.CYAN)
      g.fillOval(
        x * PieceSize,
        y * PieceSize,
        PieceSize, PieceSize)
    }
  }

  def draw(g: Graphics2D, maze: Maze, player: Player, ql: QL): Unit = {
    drawBoard(g, maze, ql)
    drawGoal(g, maze)
    drawPlayer(g, maze, player)
  }
}
