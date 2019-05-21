package com.binbo_kodakusan

case class Learning(maze: Maze, player: Player, ql: QL) {
  var step = 0

  def oneStep(learning: Boolean, goals: Int): Boolean = {
    // 方策を決定する
    val oldX = player.px
    val oldY = player.py
    val dir = ql.measureAction(maze, player, learning)
    val nowQValue = ql.getQValue(oldX, oldY, dir)
    player.move(maze, dir)
    val nextQValue = ql.getMaxQValue(player.px, player.py)
    if (maze.Goal == (player.px, player.py)) {
      // ゴールした
      // Q値を更新する(ゴール報酬)
      val newQValue = (1.0d - ql.Alpha) * nowQValue + ql.Alpha * ql.GoalReward
//      val newQValue =   nowQValue + ql.Alpha * ql.GoalReward
      ql.setQValue(oldX, oldY, dir, newQValue)
      player.reset()
      step = 0
      true
    } else {
      // Q値を更新する(次のQ値との差分)
      val newQValue = (1.0d - ql.Alpha) * nowQValue + ql.Alpha * (ql.Gamma * (nextQValue - nowQValue))
      ql.setQValue(oldX, oldY, dir, newQValue)
      step += 1
      false
    }
  }
}
