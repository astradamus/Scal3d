package com.alexanderstrada.space3d.tuple3d

import com.alexanderstrada.space3d.Axis._
import com.alexanderstrada.space3d.tuple3d.Tuple3dD._

case class Tuple3dD(override val x: Double = 0,
                    override val y: Double = 0,
                    override val z: Double = 0) extends Tuple3d[Double](x, y, z) {


  def distance(dX: Double, dY: Double, dZ: Double): Double = {
    val d = Tuple3dD(x - dX, y - dY, z - dZ)
    Math.sqrt(d.x * d.x + d.y * d.y + d.z * d.z)
  }

  def distance(t3dd: Tuple3dD): Double = distance(t3dd.x, t3dd.y, t3dd.z)

  def magnitude = distance(0, 0, 0)

  def absoluteLowest = {
    val absLowestAxis = map((ax, v) => (ax, Math.abs(v))).toSeq.sortBy(_._2).head._1
    (absLowestAxis, apply(absLowestAxis))
  }

  def +(that: Tuple3dD) = Tuple3dD(x + that.x, y + that.y, z + that.z)
  def -(that: Tuple3dD) = Tuple3dD(x - that.x, y - that.y, z - that.z)
  def *(that: Tuple3dD) = Tuple3dD(x * that.x, y * that.y, z * that.z)
  def /(that: Tuple3dD) = Tuple3dD(x / that.x, y / that.y, z / that.z)

  def +(a: Double): Tuple3dD = this + Tuple3dD(a, a, a)
  def -(s: Double): Tuple3dD = this - Tuple3dD(s, s, s)
  def *(m: Double): Tuple3dD = this * Tuple3dD(m, m, m)
  def /(d: Double): Tuple3dD = this / Tuple3dD(d, d, d)

}

sealed trait HasZero {
  val ZERO = new Tuple3dD(0, 0, 0)
}

object Tuple3dD extends HasZero {

  def apply(map: Map[Axis, Double]) = new Tuple3d(map.getOrElse(X, 0.0),
                                                  map.getOrElse(Y, 0.0),
                                                  map.getOrElse(Z, 0.0))

  type Point3dD = Tuple3dD
  type Vector3dD = Tuple3dD
  type Size3dD = Tuple3dD

  implicit def fromGeneric(t3d: Tuple3d[Double]): Tuple3dD = new Tuple3dD(t3d.x, t3d.y, t3d.z)
}


object Point3dD extends HasZero {
  def apply(x: Double = 0, y: Double = 0, z: Double = 0) = new Point3dD(x, y, z)
}

object Vector3dD extends HasZero {
  def apply(x: Double = 0, y: Double = 0, z: Double = 0) = new Vector3dD(x, y, z)
}

object Size3dD extends HasZero {
  def apply(x: Double = 0, y: Double = 0, z: Double = 0) = new Size3dD(x, y, z)
}