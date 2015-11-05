package com.alexanderstrada.graphics.simple_orthographic

import com.alexanderstrada.graphics.Rectangle
import com.alexanderstrada.space3d.tuple3d.Tuple3dD

/**
 * Defines an orthographic perspective on a 3d scene that rotates only about the X axis.
 *
 * @param screen Position and size of the camera's view of screen space.
 * @param rotationFactor Rotation about the X axis. 1.0 is top-down, 0.0 is side-on.
 * @param zoomFactor The orthographic scale, the amount by which space will be scaled.
 */
case class OrthographicCamera(screen: Rectangle = Rectangle(0, 0, 1600, 900),
                              rotationFactor: Double = 0.5,
                              zoomFactor: Double = 1) {

  /**
   * Returns a copy of the given Tuple3dD transformed according to this camera's parameters.
   */
  def project(t3dd: Tuple3dD) = Tuple3dD(t3dd.x,
                                         t3dd.y * rotationFactor,
                                         t3dd.z * (1.0 - rotationFactor)) * zoomFactor

  /**
   * Returns the amount by which the Y origin of screen space should be displaced to compensate for
   * rotation.
   */
  def depthCompensation = screen.height - screen.height * rotationFactor
}
