package com.alexanderstrada.graphics.face_renderer

import java.awt.{Color, Image}

import com.alexanderstrada.graphics.Rectangle

/**
 * Defines a rectangle to be drawn in screen space.
 *
 * @param rectangle The shape to be drawn.
 * @param sortDepth The depth at which the shape should be placed when sorted with other faces.
 * @param fill An optional fill color for the shape. Will be drawn first.
 * @param outline An optional outline color for the shape. Will be drawn second.
 * @param image An optional image for the shape. Will be drawn last.
 */
case class Face(rectangle: Rectangle,
                sortDepth: Double,
                fill: Option[Color],
                outline: Option[Color],
                image: Option[Image])