package com.alexanderstrada.space3d

import com.alexanderstrada.space3d.tuple3d.{Size3dD, Point3dD}
import com.alexanderstrada.space3d.tuple3d.Tuple3dD._

/**
  *
  */
case class Plane(origin: Point3dD,
                 size: Size3dD) {


  val perpendicularTo = size.toSeq.zip(Axis.AXES.toSeq).sortBy(_._1).head match {
    case sa if sa._1 == 0.0 =>
      sa._2
    case _ =>
      throw new IllegalArgumentException("Planes must specify 0.0 on one axis of their size.")
  }


  /**
    * Returns a copy of this with its origin offset by v.
    */
  def movedBy(v: Vector3dD) = copy(origin = origin + v)


  /**
    * Returns a copy of this with s added to its size.
    */
  def expandedBy(s: Size3dD) = copy(size = size + s)
}

object Plane {
  val ZERO = Plane(Point3dD.ZERO, Size3dD.ZERO)
}