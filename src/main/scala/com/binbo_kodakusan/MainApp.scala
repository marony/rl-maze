package com.binbo_kodakusan

import java.awt.BasicStroke
import java.awt.event.{ActionEvent}

import swing._

object MainApp extends SimpleSwingApplication {

  import java.awt.{ Color => AWTColor }

  val MazeWidth = 20
  val MazeHeight = 20
  val PieceSize = 50

  val maze = Maze(MazeWidth, MazeHeight)
  maze.construct
  val ql = QL(MazeWidth, MazeHeight)
  val player = Player(0, 0)

  var step = 0

  private def start(): Unit = {
    // 方策を決定する
    val oldX = player.px
    val oldY = player.py
    val dir = ql.measureAction(maze, player)
    val nowQValue = ql.getQValue(oldX, oldY, dir)
    player.move(maze, dir)
    val nextQValue = ql.getMaxQValue(player.px, player.py)
    if (maze.Goal == (player.px, player.py)) {
      // ゴールした
      // Q値を更新する(ゴール報酬)
      val newQValue = (1.0d - ql.Alpha) * nowQValue + ql.Alpha * ql.GoalReward
      ql.setQValue(oldX, oldY, dir, newQValue)
      player.reset()
      step = 0
    } else {
      // Q値を更新する(次のQ値との差分)
      val newQValue = (1.0d - ql.Alpha) * nowQValue + ql.Alpha * (ql.Gamma * (nextQValue - nowQValue))
      println(s"($oldX, $oldY)->(${player.px}, ${player.py}), newValue = $newQValue, nowValue = $nowQValue, newQValue = $newQValue")
      println(s"1 = ${(1.0d - ql.Alpha)}, 2 = ${(nextQValue - nowQValue)}, 3 = ${ql.Gamma * (nextQValue - nowQValue)}, 4 = ${ql.Alpha * (ql.Gamma * (nextQValue - nowQValue))}")
      ql.setQValue(oldX, oldY, dir, newQValue)
      step += 1
    }
//    println(s"dir = $dir, px = ${player.px}, py = ${player.py}, q = $nowQValue -> ${ql.getQValue(oldX, oldY, dir)}")
//    println(s"$step: q = $nowQValue -> ${ql.getQValue(oldX, oldY, dir)}")
  }

  private def drawBoard(g: Graphics2D, size: Dimension): Unit = {
    // 背景
    g.setColor(AWTColor.BLACK)
    g.fillRect(0, 0, size.width, size.height)

    // 盤面
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

  private def drawGoal(g: Graphics2D): Unit = {
    // スタートとゴール
    {
      val x = 0
      val y = 0
      g.setColor(AWTColor.BLUE)
      g.fillOval(
        x * PieceSize,
        y * PieceSize,
        PieceSize, PieceSize)
    }
    {
      val x = MazeWidth - 1
      val y = MazeHeight - 1
      g.setColor(AWTColor.RED)
      g.fillOval(
        x * PieceSize,
        y * PieceSize,
        PieceSize, PieceSize)
    }
  }

  private def drawPlayer(g: Graphics2D): Unit = {
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

  private def draw(g: Graphics2D, size: Dimension): Unit = {
    drawBoard(g, size)
    drawGoal(g)
    drawPlayer(g)
  }

  def onPaint(g: Graphics2D, size: Dimension): Unit = {
    draw(g, size)
  }

  override def top: Frame = new MainFrame {
    title = "Reinforcement Learning"
    contents = mainPanel
  }

  def mainPanel = new Panel {
    preferredSize = new Dimension(MazeWidth * PieceSize, MazeHeight * PieceSize)

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      onPaint(g, size)
    }

    val timer = new javax.swing.Timer(
      1, (actionEvent: ActionEvent) => {
        repaint()
      }
    )
    timer.start()
  }

//  val timer = new java.util.Timer
//  timer.scheduleAtFixedRate(new java.util.TimerTask {
//    override def run(): Unit = start()
//  }, 0, 1)
  val t = new Thread(() => {
    while (true) {
      start()
      Thread.`yield`()
    }
  })
  t.start()
}
