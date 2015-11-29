package com.alexanderstrada.graphics.simple_orthographic

import java.awt.{Color, Image}

import com.alexanderstrada.graphics.Rectangle
import com.alexanderstrada.graphics.face_renderer.Face
import com.alexanderstrada.space3d.Axis
import com.alexanderstrada.space3d.Side._
import com.alexanderstrada.space3d.tuple3d.Tuple3dD.Point3dD

/**
 * Produces an orthographic view of the given drawables. This view is aligned on the Y and Z axes,
 * rotating only about the X axis. This means all boxes can be expressed simply as a "top" face and
 * a "front" face, as the left and right faces will be side-on, and the bottom and back faces will
 * be occluded by the front and top faces.
 */
object SimpleOrthographic {


  def makeFace(origin: Point3dD,
               width: Double,
               height: Double,
               outline: Option[Color],
               fill: Option[Color],
               image: Option[Image],
               sortDepthAdjust: Double = 0.0,
               cam: OrthographicCamera): Face = {

    val projectedOrigin = cam.project(origin)

    implicit def toInt(d: Double): Int = d.round.asInstanceOf[Int]

    val r = Rectangle(projectedOrigin.x,
                      projectedOrigin.y - projectedOrigin.z,
                      width,
                      height)

    val adjustedForCamPosition = r -(cam.screen.x, cam.screen.y)

    val compensatedForProjection = adjustedForCamPosition + (y = cam.depthCompensation)

    val sortDepth = origin.y + origin.z + sortDepthAdjust

    Face(compensatedForProjection, sortDepth, fill, outline, image)
  }


  def apply(p: PlaneDrawable, cam: OrthographicCamera): Option[Face] = {

    val perpendicularTo = p.plane.perpendicularTo

    if (perpendicularTo == Axis.X) {
      None
    }
    else {

      val projectedSize = cam.project(p.plane.size)

      val height = projectedSize(perpendicularTo match {
                                   case Axis.Y => Axis.Z
                                   case Axis.Z => Axis.Y
                                 })

      Option(makeFace(origin = p.plane.origin,
                      width = projectedSize.x,
                      height = height,
                      outline = p.outline,
                      fill = p.fill,
                      image = p.image,
                      cam = cam))
    }
  }


  def apply(d: BoxDrawable, cam: OrthographicCamera) = {

    val projectedSize = cam.project(d.box.size)

    val topFace = makeFace(origin = d.box.vertex(LEFT, BACK, TOP),
                           width = projectedSize.x,
                           height = projectedSize.y,
                           outline = d.outline,
                           fill = d.fill,
                           image = d.topImage,
                           cam = cam)

    val frontFace = makeFace(origin = d.box.vertex(LEFT, FRONT, TOP),
                             width = projectedSize.x,
                             height = projectedSize.z,
                             outline = d.outline map (_.darker.darker.darker),
                             fill = d.fill map (_.darker),
                             image = d.frontImage,
                             sortDepthAdjust = -d.box.size.z / 2,
                             cam = cam)

    Seq(topFace, frontFace)
  }

  def apply(drawables: Seq[BoxDrawable], cam: OrthographicCamera): Seq[Face] =
    drawables flatMap (d => apply(d, cam))
}
