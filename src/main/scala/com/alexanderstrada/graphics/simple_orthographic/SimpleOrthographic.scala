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

  /**
   * The value by which top faces' sorting positions are adjusted, to give them priority over front
   * faces at identical depths. Without this, when a box is in surface contact with a box behind it,
   * the top face of the front box would sometimes be placed behind the front face of the rear box.
   */
  private val topFaceDepthPriorityNudge = 0.00001

  /**
   * The value by which the Z coordinate of a face's origin will be multiplied when its sorting
   * depth is calculated. Controls the influence of vertical position on sorting order.
   */
  private val sortDepthZFactor = 0.1


  def apply(camera: OrthographicCamera, zoneDrawables: Seq[BoxDrawable]): Seq[Face] = {

    implicit def toInt(d: Double): Int = d.round.asInstanceOf[Int]

    def toFaces(r: BoxDrawable): Seq[Face] = {
      val size = camera.project(r.box.size)

      val sortOrigin = r.box.vertex(LEFT, BACK, TOP)
      val sortDepth = sortOrigin.y + (sortOrigin.z * sortDepthZFactor)

      def makeFace(worldOrigin: Point3dD,
                   height: Double,
                   outline: Option[Color],
                   fill: Option[Color],
                   image: Option[Image],
                   sortDepthAdjustment: Double = 0.0): Face = {

        val projOrigin = camera.project(worldOrigin)

        val rect = (Rectangle(projOrigin.x, projOrigin.y - projOrigin.z, size.x, height)
                    -(camera.screen.x, camera.screen.y) // Adjust for camera position.
                    + (y = camera.depthCompensation)) // Adjust for depth projection.

        Face(rect, sortDepth + sortDepthAdjustment, outline, fill, image)
      }

      val topFace = makeFace(worldOrigin = r.box.vertex(LEFT, BACK, TOP),
                             height = size.y,
                             outline = r.outline,
                             fill = r.fill,
                             image = r.topImage,
                             sortDepthAdjustment = topFaceDepthPriorityNudge)

      val frontFace = makeFace(worldOrigin = r.box.vertex(LEFT, FRONT, TOP),
                               height = size.z,
                               outline = r.outline map (_.darker.darker.darker),
                               fill = r.fill map (_.darker.darker),
                               image = r.frontImage)

      Seq(topFace, frontFace)
    }

    zoneDrawables flatMap toFaces
  }
}
