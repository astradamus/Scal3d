package com.alexanderstrada.simple_physics

import com.alexanderstrada.space3d.tuple3d.Vector3dD
import com.alexanderstrada.space3d.{HasBox, Box}
import com.alexanderstrada.space3d.tuple3d.Tuple3dD.Vector3dD

/**
 * Defines a box that can be interpreted by SimplePhysics.
 *
 * @param box The position/size of this entity.
 * @param speed Defined in millimeters per square millisecond.
 */
case class BoxPhysical(box: Box,
                       speed: Vector3dD)

  extends HasBox {

  type Self = BoxPhysical


  /**
   * Returns a copy of this with its box replaced by the given one.
   */
  override def withBox(b: Box): BoxPhysical = if (b != box) copy(box = b) else this


  /**
   * Returns a copy of this with the given vector added to its speed.
   * @param v Defined in millimeters per millisecond.
   */
  def acceleratedBy(v: Vector3dD) = if (v != Vector3dD.ZERO) copy(speed = speed + v) else this


  /**
   * Returns a copy of this with the given vector subtracted from its speed.
   * @param v Defined in millimeters per millisecond.
   */
  def deceleratedBy(v: Vector3dD) = if (v != Vector3dD.ZERO) copy(speed = speed - v) else this
}