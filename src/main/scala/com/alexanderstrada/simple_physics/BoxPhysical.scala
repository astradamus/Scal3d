package com.alexanderstrada.simple_physics

import com.alexanderstrada.simple_physics.BoxPhysical._
import com.alexanderstrada.space3d.tuple3d.Tuple3dD.Vector3dD
import com.alexanderstrada.space3d.tuple3d.Vector3dD
import com.alexanderstrada.space3d.{Box, HasBox}

/**
 * Defines a box that can be interpreted by SimplePhysics.
 *
 * @param box Position and size.
 * @param speed Defined in millimeters per square millisecond.
 * @param solidity Optionally defines solidity for this BoxPhysical, which is used for collision.
 *                 Solidity comes in two forms: MobileSolid and BlockSolid. BlockSolids ignore each
 *                 other, MobileSolids repel each other, and BlockSolids obstruct MobileSolids.
 */
case class BoxPhysical(box: Box,
                       speed: Vector3dD,
                       solidity: Option[Solidity] = None) extends HasBox {

  type Self = BoxPhysical


  /**
   * Returns a copy of this with its box replaced by the given one.
   */
  override def withBox(b: Box): BoxPhysical = if (b != box) copy(box = b) else this


  /**
   * Returns a copy of this with its speed replaced by the one given.
   */
  def withSpeed(s: Vector3dD): BoxPhysical = if (s != speed) copy(speed = s) else this


  /**
   * Returns a copy of this with the given vector added to its speed.
   * @param v Defined in millimeters per millisecond.
   */
  def acceleratedBy(v: Vector3dD) = if (v != Vector3dD.ZERO) withSpeed(speed + v) else this


  /**
   * Returns a copy of this with the given vector subtracted from its speed.
   * @param v Defined in millimeters per millisecond.
   */
  def deceleratedBy(v: Vector3dD) = if (v != Vector3dD.ZERO) withSpeed(speed - v) else this
}

object BoxPhysical {

  /** See [[com.alexanderstrada.simple_physics.BoxPhysical]] */
  sealed trait Solidity

  case class MobileSolid(weight: Double) extends Solidity
  case object BlockSolid extends Solidity
}