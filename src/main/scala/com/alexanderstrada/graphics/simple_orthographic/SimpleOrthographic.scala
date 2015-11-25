package com.alexanderstrada.graphics.simple_orthographic

import java.awt.{Color, Image}

import com.alexanderstrada.graphics.Rectangle
import com.alexanderstrada.graphics.face_renderer.Face
import com.alexanderstrada.space3d.Side._
import com.alexanderstrada.space3d.tuple3d.Tuple3dD.Point3dD

/**
 * Produces an orthographic view of the given drawables. This view is aligned on the Y and Z axes,
 * rotating only about the X axis. This means all boxes can be expressed simply as a "top" face and
 * a "front" face, as the left and right faces will be side-on, and the bottom and back faces will
 * be occluded by the front and top faces.
 */
object SimpleOrthographic {


  def makeFace(projectedOrigin: Point3dD,
               width: Double,
               height: Double,
               outline: Option[Color],
               fill: Option[Color],
               image: Option[Image],
               sortDepth: Double = 0.0,
               cam: OrthographicCamera): Face = {


    implicit def toInt(d: Double): Int = d.round.asInstanceOf[Int]

    val r = Rectangle(projectedOrigin.x,
                      projectedOrigin.y - projectedOrigin.z,
                      width,
                      height)

    val adjustedForCamPosition = r -(cam.screen.x, cam.screen.y)

    val compensatedForProjection = adjustedForCamPosition + (y = cam.depthCompensation)

    Face(compensatedForProjection, sortDepth, outline, fill, image)
  }


  def apply(d: BoxDrawable, cam: OrthographicCamera) = {

    val sortDepthZFactor = 0.1

    val sortOrigin = d.box.vertex(LEFT, BACK, TOP)
    val sortDepth = sortOrigin.y + (sortOrigin.z * sortDepthZFactor)

    val projectedSize = cam.project(d.box.size)

    val topFace = makeFace(projectedOrigin = cam.project(d.box.vertex(LEFT, BACK, TOP)),
                           width = projectedSize.x,
                           height = projectedSize.y,
                           outline = d.outline,
                           fill = d.fill,
                           image = d.topImage,
                           sortDepth = sortDepth + 0.00001, // Top faces receive priority over front
                                                            // at same depth, prevents Z-fighting
                                                            // between faces of Y-adjacent boxes.
                           cam = cam)

    val frontFace = makeFace(projectedOrigin = cam.project(d.box.vertex(LEFT, FRONT, TOP)),
                             width = projectedSize.x,
                             height = projectedSize.z,
                             outline = d.outline map (_.darker.darker.darker),
                             fill = d.fill map (_.darker.darker),
                             image = d.frontImage,
                             sortDepth = sortDepth + projectedSize.y,
                             cam = cam)

    Seq(topFace, frontFace)
  }

  def apply(drawables: Seq[BoxDrawable], cam: OrthographicCamera): Seq[Face] =
    drawables flatMap (d => apply(d, cam))
}
