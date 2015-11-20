package com.alexanderstrada.simple_physics

import com.alexanderstrada.simple_physics.BoxPhysical._
import com.alexanderstrada.space3d.Box
import com.alexanderstrada.space3d.tuple3d.{Tuple3dD, Vector3dD}

/**
 * Produces an effective approximation of 3d physics for axis-aligned boxes, whose properties must
 * be defined in terms of instances of the BoxPhysical case class.
 */
object SimplePhysics {

  val gravityInMmPerMs2 = Vector3dD(z = -0.0098)
  val frictionAsFractionOfSpeedLostPerMs = 0.003
  val minimumSpeedPerAxisInMs2 = 0.05


  /**
   * Returns a copy of p with its speed negated on each axis upon which its box differs from
   * oldPosition. Used when positions are corrected during collision with world bounds or block
   * solids--colliding with either of these causes physicals to lose their speed on the axis of
   * collision.
   */
  def negateSpeedOnChangedAxes(p: BoxPhysical, oldPosition: Box): BoxPhysical = {

    val haltingVector = p.speed.map((axis, speedOnAxis) => {
      if (oldPosition.origin(axis) != p.box.origin(axis))
        speedOnAxis
      else
        0.0
    })

    p.deceleratedBy(haltingVector)
  }


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

    val pWithBoundBox = p.withBox(boxAfterBind)

    negateSpeedOnChangedAxes(pWithBoundBox, boxBeforeBind)
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


  /**
   * Applies gravity, speed, boundaries, friction, and minimum speed to each BoxPhysical in the
   * given sequence, returning the results as a parallel sequence.
   */
  def applyMotionPhysics(deltaTimeInMs: Long,
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


  /**
   * Returns a pair of BoxPhysicals that are copies of solid1 and solid2, but with their speeds
   * adjusted so that they are repelled by one another. They will move opposite each other on the
   * axis upon which they intersect least, and the total magnitude of the repulsion will be the
   * amount that they intersect on that axis (the more they are intersecting, the stronger the
   * repulsion). Note that this means if the given physicals do NOT intersect, they will instead
   * attract each other.<br><br>
   *
   * The proportion of this force each physical experiences is inversely proportional to
   * its weight, so that the lighter of the two is moved more.
   */
  def applySolidRepulsion(deltaTimeInMs: Long,
                          solid1: BoxPhysical,
                          solid2: BoxPhysical) = {

    val compare = solid1.box.compareTo(solid2.box) / 1000 * deltaTimeInMs

    val (s1Weight, s2Weight) = (solid1.solidity, solid2.solidity) match {
      case (Some(s1: MobileSolid), Some(s2: MobileSolid)) => (s1.weight, s2.weight)
      case _ => throw new IllegalArgumentException
    }

    val weights = s1Weight + s2Weight
    val ratios = (s2Weight / weights,
                  s1Weight / -weights)

    val absLowest = compare.absoluteLowest

    def repel(solid: BoxPhysical, ratio: Double) = {
      solid.acceleratedBy(Tuple3dD(Map(absLowest._1 -> absLowest._2 * ratio)))
    }

    (repel(solid1, ratios._1),
      repel(solid2, ratios._2))
  }


  /**
   * Returns a copy of mobileSolid moved the least possible amount so that it intersects blockSolid
   * inclusively, but NOT exclusively (i.e. they are only intersecting at surface--they are
   * 'touching'). The copy's speed will be negated on the axis upon which it is displaced.
   */
  def applySolidObstruction(mobileSolid: BoxPhysical, blockSolid: BoxPhysical) = {

    val compare = mobileSolid.box.compareTo(blockSolid.box)
    val vector = Tuple3dD(Map(compare.absoluteLowest))

    val boxBeforeObstruct = mobileSolid.box
    val boxAfterObstruct = boxBeforeObstruct movedBy vector

    negateSpeedOnChangedAxes(mobileSolid.withBox(boxAfterObstruct),
                             boxBeforeObstruct)
  }


  /**
   * Returns a set containing all colliding pairs in the given sequence, defined in terms of each
   * BoxPhysical's index within the sequence. Two BoxPhysicals are colliding if they are both solid
   * and they exclusively intersect.
   */
  def evaluateCollisions(inPhysicals: Seq[BoxPhysical]): Set[(Int, Int)] = {
    val empty = Set.empty[(Int, Int)]

    val solidPhysicals = inPhysicals filter (_.solidity.isDefined)

    solidPhysicals.foldLeft(empty)((allIntersections, thisPhysical) => {

      def eval(all: Set[(Int, Int)], otherPhysical: BoxPhysical) = {

        if (thisPhysical.box exclusiveIntersects otherPhysical.box) {

          val thisIndex = inPhysicals.indexOf(thisPhysical)
          val otherIndex = inPhysicals.indexOf(otherPhysical)

          all + ((thisIndex, otherIndex))

        }
        else {
          all
        }
      }

      val remainingPhysicals = solidPhysicals.drop(solidPhysicals.indexOf(thisPhysical) + 1)
      allIntersections ++ remainingPhysicals.foldLeft(empty)(eval)
    })
  }


  /**
   * Applies solid repulsion to each applicable BoxPhysical in the given sequence, returning the
   * results as a parallel sequence.
   */
  def applyCollisionPhysics(deltaTimeInMs: Long,
                            inPhysicals: Seq[BoxPhysical]): Seq[BoxPhysical] = {

    evaluateCollisions(inPhysicals).foldLeft(inPhysicals)((outPhysicals, collision) => {

      val solid1Index = collision._1
      val solid2Index = collision._2

      val inSolid1 = outPhysicals(collision._1)
      val inSolid2 = outPhysicals(collision._2)

      val (outSolid1, outSolid2) = (inSolid1.solidity.get, inSolid2.solidity.get) match {

        // Blocks ignore each other.
        case (s1, s2) if s1 == BlockSolid && s2 == BlockSolid =>
          (inSolid1, inSolid2)

        // Mobiles repel each other.
        case (s1, s2) if s1 != BlockSolid && s2 != BlockSolid =>
          applySolidRepulsion(deltaTimeInMs, inSolid1, inSolid2)

        // Blocks obstruct mobiles.
        case (s1, s2) if s1 == BlockSolid =>
          (inSolid1, applySolidObstruction(inSolid2, inSolid1))

        // Blocks obstruct mobiles.
        case (s1, s2) if s2 == BlockSolid =>
          (applySolidObstruction(inSolid1, inSolid2), inSolid2)
      }

      outPhysicals
        .updated(solid1Index, outSolid1)
        .updated(solid2Index, outSolid2)
    })
  }


  def apply(deltaTimeInMs: Long,
            inPhysicals: Seq[BoxPhysical],
            worldBounds: Option[Box] = None): Seq[BoxPhysical] = {

    var outPhysicals = inPhysicals

    outPhysicals = applyMotionPhysics(deltaTimeInMs, outPhysicals, worldBounds)
    outPhysicals = applyCollisionPhysics(deltaTimeInMs, outPhysicals)

    outPhysicals
  }
}
