package com.alexanderstrada.graphics.simple_orthographic

import com.alexanderstrada.space3d.Plane

/**
 * Defines a plane that can be interpreted by SimpleOrthographic and then drawn by FaceRenderer.
 *
 * @param plane The shape to be interpreted.
 * @param drawable The visual details of the shape.
 */
case class PlaneDrawable(plane: Plane = Plane.ZERO,
                         drawable: Drawable) {

  /**
   * Returns a copy of this PlaneDrawable with its plane replaced by the one given.
   */
  def withPlane(p: Plane) = if (p != plane) copy(plane = p) else this
}