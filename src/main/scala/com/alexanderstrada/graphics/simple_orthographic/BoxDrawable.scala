package com.alexanderstrada.graphics.simple_orthographic

import java.awt.{Color, Image}

import com.alexanderstrada.space3d.{HasBox, Box}

/**
 * Defines a box that can be interpreted by SimpleOrthographic and then drawn by FaceRenderer.
 *
 * @param box The shape to be interpreted.
 * @param drawable The visual details of the shape.
 */
case class BoxDrawable(box: Box = Box.ZERO,
                       drawable: Drawable) extends HasBox {

  type Self = BoxDrawable


  /**
   * Returns a copy of this BoxDrawable with its box replaced by the given one.
   */
  override def withBox(b: Box) = if (b != box) copy(box = b) else this
}