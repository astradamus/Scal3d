package com.alexanderstrada.space3d

import com.alexanderstrada.space3d.Side._
import com.alexanderstrada.space3d.tuple3d.Tuple3d

object Axis {

  sealed abstract class Axis private[space3d] {
    def min: Side
    def max: Side
  }

  val X = new Axis {
    override def min = LEFT
    override def max = RIGHT
  }

  val Y = new Axis {
    override def min = BACK
    override def max = FRONT
  }

  val Z = new Axis {
    override def min = BOTTOM
    override def max = TOP
  }

  val AXES: Tuple3d[Axis] = Tuple3d(X, Y, Z)
}
