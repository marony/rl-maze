package com.binbo_kodakusan

import java.awt.BasicStroke

import swing._

object MainApp extends SimpleSwingApplication {

  import java.awt.{ Color => AWTColor }

  override def top: Frame = new MainFrame {
    title = "Reinforcement Learning"
    contents = mainPanel
  }

  val MazeWidth = 20
  val MazeHeight = 20
  val PieceSize = 20

  val maze = Maze(MazeWidth, MazeHeight)
  maze.construct

  def onPaint(g: Graphics2D): Unit = {
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

  def mainPanel = new Panel {
    preferredSize = new Dimension(MazeWidth * PieceSize, MazeHeight * PieceSize)

    override def paint(g: Graphics2D): Unit = {
      g.setColor(AWTColor.BLACK)
      g.fillRect(0, 0, size.width, size.height)
      onPaint(g)
    }
  }
}
