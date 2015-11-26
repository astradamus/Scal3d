package com.alexanderstrada.graphics.simple_orthographic

import java.awt.{Color, Image}

import com.alexanderstrada.space3d.Plane

/**
 * Defines a plane that can be interpreted by SimpleOrthographic and then drawn by FaceRenderer.
 *
 * @param plane The shape to be interpreted.
 * @param outline An optional color for the plane's edges.
 * @param fill An optional color for the plane.
 * @param image An optional image for the plane.
 */
case class PlaneDrawable(plane: Plane = Plane.ZERO,
                         outline: Option[Color] = None,
                         fill: Option[Color] = None,
                         image: Option[Image] = None) {

  /**
   * Returns a copy of this PlaneDrawable with its plane replaced by the one given.
   */
  def withPlane(p: Plane) = if (p != plane) copy(plane = p) else this
}