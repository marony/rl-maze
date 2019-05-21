package com.binbo_kodakusan

import java.awt.event.{ActionEvent}
import swing._

import Const._

object MainApp extends SimpleSwingApplication {

  // 迷路
  val maze = Maze(MazeWidth, MazeHeight)
  // Q値
  val ql = QL(MazeWidth, MazeHeight)
  // プレイヤー
  val player = Player(0, 0)
  // 強化学習君
  val learn = Learning(maze, player, ql)

  def onPaint(g: Graphics2D, size: Dimension): Unit = {
    View.draw(g, maze, player, ql)
  }

  // 表示用ラベル
  def label = new Label {

    font = new Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 24)
    text = "*****"

    // 描画タイマー
    val timer = new javax.swing.Timer(
      10, (actionEvent: ActionEvent) => {
//        val q = ql.getQValue(player.px, player.py)
//        val s = s"(${q(0)}, ${q(1)}, ${q(2)}, ${q(3)})"
        val s = ql.getMaxQValue(player.px, player.py)
        text = s"(${player.px}, ${player.py}) = ${s}"
        repaint()
      }
    )
    timer.start()
  }

  // 描画用パネル
  def mainPanel = new Panel {
    preferredSize = new Dimension(MazeWidth * PieceSize + 1, MazeHeight * PieceSize + 1)

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      onPaint(g, size)
    }

    // 描画タイマー
    val timer = new javax.swing.Timer(
      10, (actionEvent: ActionEvent) => {
        repaint()
      }
    )
    timer.start()
  }

  // メインフレーム
  override def top: Frame = new MainFrame {
    title = "Reinforcement Learning"
    contents = new BoxPanel(Orientation.Vertical) {
      contents += label
      contents += mainPanel
    }
  }
  top.pack()

  // 学習タイマー
//  val timer = new java.util.Timer(true)
//  timer.scheduleAtFixedRate(new java.util.TimerTask {
//    override def run(): Unit = learn.oneStep()
//  }, 100, 1)
  val t = new Thread(() => {
    Thread.sleep(1000 * 30)
    while (true) {
      for (i <- 1 to 1) {
        learn.oneStep()
      }
      Thread.sleep(1)
//      Thread.`yield`()
    }
  })
  t.start()

  override def shutdown(): Unit = {
//    timer.cancel()
    super.shutdown()
  }
}
