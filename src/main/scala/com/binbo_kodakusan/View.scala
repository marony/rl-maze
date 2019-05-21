package com.binbo_kodakusan

import java.awt.BasicStroke
import java.awt.{Color => AWTColor}

import scala.swing.{Dimension, Font, Graphics2D}
import Const._

object View {

  private val FontSize = 16

  private def drawBoard(g: Graphics2D, maze: Maze, ql: QL, learning: Boolean, goals: Int): Unit = {
    // 背景
    g.setColor(AWTColor.BLACK)
    g.fillRect(0, 0, maze.width * PieceSize, maze.height * PieceSize)

    // 盤面
    for (y <- 0 until MazeWidth) {
      for (x <- 0 until MazeHeight) {

        val x1 = x * PieceSize
        val y1 = y * PieceSize
        val x2 = (x + 1) * PieceSize
        val y2 = (y + 1) * PieceSize

        // 最大Q値で塗り潰し
        // 0〜225にする
        {
          val qs = ql.getQValue(x, y).zipWithIndex.sortBy{case (q, i) => q}.reverse;
          {
            // 左
            val i = qs.dropWhile{case (q, i) => i != 0}.length - 1
            val c = 48 * i
            g.setColor(new AWTColor(0, c, 0))
            g.fillRect(x1, y1 + 6, 10, PieceSize - 10)
          }
          {
            // 上
            val i = qs.dropWhile{case (q, i) => i != 1}.length - 1
            val c = 48 * i
            g.setColor(new AWTColor(0, c, 0))
            g.fillRect(x1 + 6, y1, PieceSize - 10, 10)
          }
          {
            // 右
            val i = qs.dropWhile{case (q, i) => i != 2}.length - 1
            val c = 48 * i
            g.setColor(new AWTColor(0, c, 0))
            g.fillRect(x2 - 10, y1 + 6, 10, PieceSize - 10)
          }
          {
            // 下
            val i = qs.dropWhile{case (q, i) => i != 3}.length - 1
            val c = 48 * i
            g.setColor(new AWTColor(0, c, 0))
            g.fillRect(x1 + 6, y2 - 6, PieceSize - 10, 10)
          }
        }
        // 数値
        {
          val BaseSize = FontSize / 2
          val qs = ql.getQValue(x, y)
          g.setColor(AWTColor.DARK_GRAY)
          g.setFont(new Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, FontSize))
          // 左
          g.drawString(qs(0).toInt.toString, x1, y1 + PieceSize / 2 + BaseSize)
          // 上
          g.drawString(qs(1).toInt.toString, x1 + PieceSize / 2 - BaseSize, y1 + FontSize)
          // 右
          g.drawString(qs(2).toInt.toString, x2 - BaseSize * 2, y1 + PieceSize / 2 + BaseSize)
          // 下
          g.drawString(qs(3).toInt.toString, x1 + PieceSize / 2 - BaseSize, y2)
        }
        // 学習中
        if (learning) {
          g.setColor(AWTColor.RED)
          g.setFont(new Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 72))
          g.drawString(s"$goals, Learning", PieceSize * maze.width / 4, PieceSize * maze.height / 2)
        }
      }
    }

    // 線
    g.setColor(AWTColor.WHITE)
    g.setStroke(new BasicStroke(3f))
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

  def draw(g: Graphics2D, maze: Maze, player: Player, ql: QL, learning: Boolean, goals: Int): Unit = {
    drawBoard(g, maze, ql, learning, goals)
    drawGoal(g, maze)
    drawPlayer(g, maze, player)
  }
}
