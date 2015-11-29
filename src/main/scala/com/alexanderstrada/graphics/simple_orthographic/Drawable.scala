package com.alexanderstrada.graphics.simple_orthographic

import java.awt.{Color, Image}

/**
 * Defines visual details that can be interpreted by SimpleOrthographic and then drawn by
 * FaceRenderer.
 *
 * @param topOutline An optional color for the drawable's top view edges.
 * @param topFill An optional color for the drawable's top view fill.
 * @param topImage An optional image for the drawable's top view.
 * @param frontOutline An optional color for the drawable's front view edges.
 * @param frontFill An optional color for the drawable's front view fill.
 * @param frontImage An optional image for the drawable's front view.
 */
case class Drawable(topOutline: Option[Color] = None,
                    topFill: Option[Color] = None,
                    topImage: Option[Image] = None,
                    frontOutline: Option[Color] = None,
                    frontFill: Option[Color] = None,
                    frontImage: Option[Image] = None)


object Drawable {

  def apply(bothOutline: Option[Color],
            bothFill: Option[Color],
            bothImage: Option[Image]): Drawable = {

    val (tOut, fOut) = bothOutline match {
      case Some(c) => (Option(c), Option(c.darker))
      case _ => (None, None)
    }

    val (tFill, fFill) = bothFill match {
      case Some(c) => (Option(c), Option(c.darker))
      case _ => (None, None)
    }

    val (tImage, fImage) = bothImage match {
      case Some(i) => (Option(i), Option(i))
      case _ => (None, None)
    }

    Drawable(tOut, tFill, tImage, fOut, fFill, fImage)
  }

  def apply(bothColor: Color): Drawable = {

    val topOutline = bothColor.darker.darker
    val topFill = bothColor

    val frontOutline = topOutline.darker
    val frontFill = topFill.darker

    Drawable(Option(topOutline), Option(topFill), None,
             Option(frontOutline), Option(frontFill), None)
  }
}