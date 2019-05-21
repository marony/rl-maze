package com.binbo_kodakusan

import scala.util.Random

case class QL(width: Int, height: Int) {
  // 初期値の最大値
  val InitMaxQl = 50d

  // 学習係数
  val Alpha = 0.2
  // 割引率
  val Gamma = 0.5
  // e-greedy法
  val Epsilon = 0.1
  // ゴールの報酬
  val GoalReward = 100.0
  // 壁だった時の報酬
  val Panishment = -10.0

  val rand: Random = new Random()

  // QLのランダムに初期化
  val qvalue: Array[Array[Double]] = (0 until width * height)
    .map(n => Array(
      rand.nextDouble() * InitMaxQl,
      rand.nextDouble() * InitMaxQl,
      rand.nextDouble() * InitMaxQl,
      rand.nextDouble() * InitMaxQl))
    .toArray

  private[this] def getIndex(x: Int, y: Int) = y * height + x
  def getQValue(x: Int, y: Int): Array[Double] = qvalue(getIndex(x, y))
  def getQValue(x: Int, y: Int, dir: Direction): Double = getQValue(x, y)(Direction.toInt(dir))
  def setQValue(x: Int, y: Int, dir: Direction, value: Double) = qvalue(getIndex(x, y))(Direction.toInt(dir)) = value

  def getMaxQValue(x: Int, y: Int): Double = {
    qvalue(getIndex(x, y)).max
  }

  /**
    * 方策を決定する
    * ε-greedy法
    *
    * @param maze
    * @param player
    */
  def measureAction(maze: Maze, player: Player, learning: Boolean): Direction = {
    def selectDir(): Direction = {
      val r = rand.nextDouble()
      if (learning && r < Epsilon) {
        // ランダムに選択する
        Direction.fromInt(rand.nextInt(4))
      } else {
        // 大きなQ値の方策を選択する
        val max = (0 until 4).map(Direction.fromInt(_))
          .maxBy(dir => getQValue(player.px, player.py, dir))
        val min = (0 until 4).map(Direction.fromInt(_))
          .minBy(dir => getQValue(player.px, player.py, dir))
        if (min == max) {
          // 全部同じ場合もやっぱりランダムに選択する
          Direction.fromInt(rand.nextInt(4))
        } else {
          max
        }
      }
    }

    var dir = selectDir()
    while (!player.movable(maze, dir)) {
      // 壁なので罰を与えて再チャレンジ
      var qs = getQValue(player.px, player.py, dir)
      qs += Alpha * Panishment
      setQValue(player.px, player.py, dir, qs)
      dir = selectDir()
    }

    dir
  }
}
