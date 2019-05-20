package com.binbo_kodakusan

import scala.util.Random

case class QL(width: Int, height: Int) {
  // 初期値の最大値
  val InitMaxQl = 50d

  // 学習係数
  val Alpha = 0.1
  // 割引率
  val Gamma = 0.5
  // e-greedy法
  val Epsilon = 0.1
  // ゴールの報酬
  val GoalReward = 100.0

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
  def measureAction(maze: Maze, player: Player): Direction = {
    val r = rand.nextDouble()
    if (r < Epsilon) {
      // ランダムに選択する
      var dir = Direction.fromInt(rand.nextInt(4))
      while (!player.movable(maze, dir)) {
        dir = Direction.fromInt(rand.nextInt(4))
      }
      dir
    } else {
      // 大きなQ値の方策を選択する
      (0 until 4).map(Direction.fromInt(_))
        .filter(player.movable(maze, _))
        .maxBy(dir => getQValue(player.px, player.py, dir))
    }
  }
}
