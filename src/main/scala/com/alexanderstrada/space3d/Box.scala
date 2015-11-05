package com.alexanderstrada.space3d

import com.alexanderstrada.space3d.Side._
import com.alexanderstrada.space3d.Axis._
import com.alexanderstrada.space3d.tuple3d._
import com.alexanderstrada.space3d.tuple3d.Tuple3dD._

/**
 * Defines an axis-aligned box in space as an origin point and a size.
 */
case class Box(origin: Point3dD = Point3dD.ZERO,
               size: Size3dD = Size3dD.ZERO) {


  /**
   * Returns the coordinate of the given side (LEFT, RIGHT, BACK, FRONT, BOTTOM, or TOP).
   */
  def apply(s: Side): Double =
    origin(s.axis) + (if (s.minOrMax == MIN) 0.0 else size(s.axis))


  /**
   * Returns a Point3dD representing the center of this box.
   */
  def center: Point3dD = origin + (size / 2)


  /**
   * Returns a Point3dD representing the vertex of this box defined by the intersection of an XSide
   * (LEFT or RIGHT), a YSide (BACK or FRONT), and a ZSide (BOTTOM or TOP).
   */
  def vertex(x: XSide, y: YSide, z: ZSide): Point3dD = Tuple3d(x, y, z) map apply _


  /**
   * Returns a sequence that contains all eight of this box's vertices, starting at the origin
   * (left back bottom), working clockwise around the bottom to (left front bottom), then straight
   * up to (left front top), and then counter-clockwise around the top to (left back top).
   */
  def vertices = Seq(vertex(LEFT, BACK, BOTTOM),
                     vertex(RIGHT, BACK, BOTTOM),
                     vertex(RIGHT, FRONT, BOTTOM),
                     vertex(LEFT, FRONT, BOTTOM),
                     vertex(LEFT, FRONT, TOP),
                     vertex(RIGHT, FRONT, TOP),
                     vertex(RIGHT, BACK, TOP),
                     vertex(LEFT, BACK, TOP))


  /**
   * Returns a copy of this box with its origin and size adjusted by the given parameters.
   */
  def +(adjOrigin: Vector3dD = Vector3dD.ZERO,
        adjSize: Size3dD = Size3dD.ZERO) = Box(origin + adjOrigin, size + adjSize)


  /**
   * Returns the smallest possible box that contains both this box and the given box.
   */
  def union(other: Box): Box = {

    val x = Math.min(this(LEFT), other(LEFT))
    val y = Math.min(this(BACK), other(BACK))
    val z = Math.min(this(BOTTOM), other(BOTTOM))

    val width = Math.max(this(RIGHT), other(RIGHT)) - x
    val depth = Math.max(this(FRONT), other(FRONT)) - y
    val height = Math.max(this(TOP), other(TOP)) - z


    Box(Point3dD(x, y, z), Size3dD(width, depth, height))
  }

  /**
   * Returns true if the given box is wholly contained inside this one. This containment is
   * inclusive, so a box is considered to contain itself.
   */
  def contains(other: Box): Boolean = {

    def contained(axis: Axis) =
      other(axis.min) >= this(axis.min) && other(axis.max) <= this(axis.max)

    AXES forall contained _
  }


  /**
   * Returns true if the given box intersects this one, including surface contact. Two boxes with
   * touching sides (they have the same coordinate on some axis) are considered to be intersecting.
   */
  def intersects(other: Box): Boolean = {

    def intersected(axis: Axis) =
      other(axis.min) <= this(axis.max) && other(axis.max) >= this(axis.min)

    AXES forall intersected _
  }


  /**
   * Returns true if the given box intersects this one at a non-surface depth. Two boxes whose sides
   * are touching (they have the same coordinate on some axis) are NOT considered to be exclusively
   * intersecting.
   */
  def exclusiveIntersects(other: Box): Boolean = {

    def exclusiveIntersected(axis: Axis) =
      other(axis.min) < this(axis.max) && other(axis.max) > this(axis.min)

    AXES forall exclusiveIntersected _
  }

  /**
   * Returns a new box with the same size as this one, but centered on (or in, if larger than) the
   * given container.
   */
  def centeredIn(container: Box) = Box(container.center - this.size/2, this.size)


  /**
   * Returns a Tuple3dD in which each value is the comparison between this box and the given box on
   * that value's axis.<br><br>
   *
   * The comparison between two boxes is the shortest distance between them on
   * an axis--it is the absolute lowest (closest to zero) value that can be added to THIS
   * box so that it will be precisely in surface contact with the given box on that axis.<br><br>
   *
   * The effect is that if this box is intersecting the given box, it will be 'shunted out,' and if
   * it is not, it will be 'dragged in.'<br><br>
   *
   * Applying all three comparison values would result in touching only at the tip of a corner. More
   * often, only one of the three values will be used at a time, to determine which axis two boxes
   * are closest or furthest on.
   */
  def compareTo(other: Box): Tuple3dD = {

    def compare(axis: Axis) = {

      val both = Seq(other(axis.min) - this(axis.max),
                     other(axis.max) - this(axis.min))
      both
        .zip(both map Math.abs)
        .sortBy(_._2)
        .head._1
    }

    AXES map compare _
  }


  override def toString: String = "Box[" + origin + ", " + size + "]"

}

object Box {
  val ZERO = Box(Point3dD.ZERO, Size3dD.ZERO)
}