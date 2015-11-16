package com.alexanderstrada.space3d

import com.alexanderstrada.space3d.tuple3d.Tuple3dD._

/**
 *
 */
trait HasBox {
  type Self <: HasBox

  def box: Box

  /**
   * Returns a copy of this object with its box's origin adjusted by the given vector.
   */
  def movedBy(v: Vector3dD): Self = this.withBox(box movedBy v)


  /**
   * Returns a copy of this object with its box replaced by the given one.
   */
  def withBox(b: Box): Self
}
