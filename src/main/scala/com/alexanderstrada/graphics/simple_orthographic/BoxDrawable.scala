package com.alexanderstrada.graphics.simple_orthographic

import java.awt.{Color, Image}

import com.alexanderstrada.space3d.{HasBox, Box}

/**
 * Defines a box that can be interpreted by SimpleOrthographic and then drawn by FaceRenderer.
 *
 * @param box The shape to be interpreted.
 * @param outline An optional color for the box's edges.
 * @param fill An optional color for the box's faces.
 * @param topImage An optional image for the box's top face.
 * @param frontImage An optional image for the box's front face.
 */
case class BoxDrawable(box: Box = Box.ZERO,
                       outline: Option[Color] = None,
                       fill: Option[Color] = None,
                       topImage: Option[Image] = None,
                       frontImage: Option[Image] = None)
  extends HasBox {

  type Self = BoxDrawable


  /**
   * Returns a copy of this BoxDrawable with its box replaced by the given one.
   */
  override def withBox(b: Box) = if (b != box) copy(box = b) else this
}