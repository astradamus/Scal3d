package com.alexanderstrada.graphics.face_renderer

import java.awt.{Color, Graphics, Image}

import com.alexanderstrada.graphics.Rectangle

/**
 * Defines drawing methods used by FaceRenderer.
 */
trait FaceCanvas {
  def setColor(c: Color)
  def outline(r: Rectangle)
  def fill(r: Rectangle)
  def drawImage(i: Image, r: Rectangle)
}

object FaceCanvas {

  implicit def wrapAWT(g: Graphics): AWTWrapper = new AWTWrapper(g)

  class AWTWrapper(g: Graphics) extends FaceCanvas {

    override def setColor(c: Color): Unit = g.setColor(c)

    override def fill(r: Rectangle): Unit = g.fillRect(r.x, r.y, r.width, r.height)

    override def outline(r: Rectangle): Unit = g.drawRect(r.x, r.y, r.width, r.height)

    override def drawImage(i: Image, r: Rectangle): Unit =
      g.drawImage(i, r.x, r.y, r.width, r.height, null)
  }
}
