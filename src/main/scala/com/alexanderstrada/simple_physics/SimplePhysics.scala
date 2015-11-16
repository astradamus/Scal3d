package com.alexanderstrada.simple_physics

import com.alexanderstrada.space3d.Box
import com.alexanderstrada.space3d.tuple3d.Vector3dD

/**
 * Produces an effective approximation of 3d physics for axis-aligned boxes, whose properties must
 * be defined in terms of instances of the BoxPhysical case class.
 */
object SimplePhysics {

  val gravityInMmPerMs2 = Vector3dD(z = -0.0098)
  val frictionAsFractionOfSpeedLostPerMs = 0.003
  val minimumSpeedPerAxisInMs2 = 0.05


  /**
   * Returns a copy of p accelerated by gravity as defined by gravityInMmPerMs2.
   */
  def applyGravity(p: BoxPhysical, deltaTimeInMs: Long) =
    p.acceleratedBy(gravityInMmPerMs2 * deltaTimeInMs)


  /**
   * Returns a copy of p with its box moved according to its speed.
   */
  def applySpeed(p: BoxPhysical, deltaTimeInMs: Long) =
    p.movedBy(p.speed * deltaTimeInMs)


  /**
   * If worldBounds is defined, applies local function bind(BoxPhysical, Box) to p and returns the
   * result. Otherwise, it returns p unchanged.
   */
  def maybeBind(p: BoxPhysical,
                worldBounds: Option[Box]): BoxPhysical = worldBounds match {
    case Some(b) => bind(p, b)
    case None => p
  }


  /**
   * Applies box function boundTo(Box) to p's box, using bounds as the parameter. If it changes
   * position on any axis (i.e. it 'runs into' the boundary walls on that axis), its speed
   * on that axis will be negated 'by the impact.'
   */
  def bind(p: BoxPhysical, bounds: Box): BoxPhysical = {
    val boxBeforeBind = p.box
    val boxAfterBind = boxBeforeBind.boundTo(bounds)

    val haltingVector = p.speed.map((axis, speedOnAxis) => {
      if (boxBeforeBind.origin(axis) != boxAfterBind.origin(axis))
        speedOnAxis
      else
        0.0
    })

    p.withBox(boxAfterBind)
     .deceleratedBy(haltingVector)
  }


  /**
   * Returns a copy of p with its speed reduced according to friction as defined by
   * frictionAsFractionOfSpeedLostPerMs.
   */
  def applyFriction(p: BoxPhysical, deltaTimeInMs: Long) = {
    val percentOfSpeedLost = frictionAsFractionOfSpeedLostPerMs * deltaTimeInMs
    p.deceleratedBy(p.speed * percentOfSpeedLost)
  }


  /**
   * Returns a copy of p with its speed reduced to zero on each axis whose value is lower than the
   * minimum as defined by minimumSpeedPerAxisInMs2. Returns p unchanged if no enforcement is
   * necessary. Ensures that moving boxes actually ever stop due to friction, instead of approaching
   * ever smaller fractional speeds.
   */
  def enforceMinimumSpeed(p: BoxPhysical) = {
    val enforced = p.speed map (v => if (v.abs < minimumSpeedPerAxisInMs2) 0.0 else v)

    if (enforced != p.speed) {
      p.copy(speed = enforced)
    }
    else p
  }


  def apply(deltaTimeInMs: Long,
            inPhysicals: Seq[BoxPhysical],
            worldBounds: Option[Box] = None): Seq[BoxPhysical] = {

    inPhysicals map (in => {

      var out = in

      out = applyGravity(out, deltaTimeInMs)
      out = applySpeed(out, deltaTimeInMs)
      out = maybeBind(out, worldBounds)
      out = applyFriction(out, deltaTimeInMs)
      out = enforceMinimumSpeed(out)

      out
    })
  }
}
