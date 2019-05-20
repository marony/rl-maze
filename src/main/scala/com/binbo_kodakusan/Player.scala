package com.binbo_kodakusan

case class Player(var px: Int, var py: Int) {

  def reset(): Unit = {
    px = 0
    py = 0
  }

  /**
    * その方向に移動できるか？
    *
    * @param maze
    * @param dir
    * @return
    */
  def movable(maze: Maze, dir: Direction): Boolean = {
    maze.movable(px, py, dir)._3 != Direction.None
  }

  def move(maze: Maze, dir: Direction): Unit = {
    val r = maze.movable(px, py, dir)
    if (r._3 == Direction.None) {
      ???
    }
    px = r._1
    py = r._2
  }
}
