package com.alexanderstrada.space3d

import com.alexanderstrada.space3d.Axis._

object Side {

  class MinOrMax private[Side] (isMax: Boolean = false)
  val MIN = new MinOrMax(false)
  val MAX = new MinOrMax(true)

  sealed case class Side(axis: Axis, minOrMax: MinOrMax)
  class XSide(mom: MinOrMax) extends Side(X, mom)
  class YSide(mom: MinOrMax) extends Side(Y, mom)
  class ZSide(mom: MinOrMax) extends Side(Z, mom)


  // ENUMERATED SIDES
  val LEFT = new XSide(MIN)
  val RIGHT = new XSide(MAX)

  val BACK = new YSide(MIN)
  val FRONT = new YSide(MAX)

  val BOTTOM = new ZSide(MIN)
  val TOP = new ZSide(MAX)


  // SIDE ALIASES
  val X_MIN = LEFT
  val X_MAX = RIGHT

  val Y_MIN = BACK
  val Y_MAX = FRONT

  val Z_MIN = BOTTOM
  val Z_MAX = TOP
}