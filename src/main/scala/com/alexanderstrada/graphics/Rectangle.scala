package com.alexanderstrada.graphics

import java.awt

/**
 * An immutable, implicitly convertible stand-in for [[java.awt.Rectangle]].
 */
case class Rectangle(x: Int, y: Int, width: Int, height: Int) {

  def +(x: Int = 0, y: Int = 0, width: Int = 0, height: Int = 0): Rectangle = {
    Rectangle(this.x + x, this.y + y, this.width + width, this.height + height)
  }

  def -(x: Int = 0, y: Int = 0, width: Int = 0, height: Int = 0): Rectangle = {
    Rectangle(this.x - x, this.y - y, this.width - width, this.height - height)
  }

  def *(x: Int = 0, y: Int = 0, width: Int = 0, height: Int = 0): Rectangle = {
    Rectangle(this.x * x, this.y * y, this.width * width, this.height * height)
  }

  def /(x: Int = 0, y: Int = 0, width: Int = 0, height: Int = 0): Rectangle = {
    Rectangle(this.x / x, this.y / y, this.width / width, this.height / height)
  }
}

object Rectangle {
  implicit def toAWTRectangle(r: Rectangle): awt.Rectangle =
    new awt.Rectangle(r.x, r.y, r.width, r.height)
}