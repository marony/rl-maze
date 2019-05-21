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

  // メインフレーム
  override def top: Frame = new MainFrame {
    title = "Reinforcement Learning"
    contents = mainPanel
  }

  // 描画用パネル
  def mainPanel = new Panel {
    preferredSize = new Dimension(MazeWidth * PieceSize, MazeHeight * PieceSize)

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      onPaint(g, size)
    }

    // 描画タイマー
    val timer = new javax.swing.Timer(
      100, (actionEvent: ActionEvent) => {
        repaint()
      }
    )
    timer.start()
  }

  // 学習タイマー
  val timer = new java.util.Timer(true)
  timer.scheduleAtFixedRate(new java.util.TimerTask {
    override def run(): Unit = learn.oneStep()
  }, 100, 500)
//  val t = new Thread(() => {
//    while (true) {
//      for (i <- 0 to 10000) {
//        learn.oneStep()
//      }
//      Thread.`yield`()
//    }
//  })
//  t.start()

  override def shutdown(): Unit = {
    timer.cancel()
    super.shutdown()
  }
}
