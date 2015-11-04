package com.alexanderstrada.graphics.simple_orthographic

import java.awt.{Color, Image}

import com.alexanderstrada.space3d.Box

/**
 * Defines a box that can be interpreted by OrthographicFaceCollector and then drawn by
 * FaceRenderer.
 *
 * @param box The shape to be interpreted.
 * @param outline An optional color for the box's edges.
 * @param fill An optional color for the box's faces.
 * @param topImage An optional image for the box's top face.
 * @param frontImage An optional image for the box's front face.
 */
case class OrthographicDrawable(box: Box = Box.ZERO,
                                outline: Option[Color] = None,
                                fill: Option[Color] = None,
                                topImage: Option[Image] = None,
                                frontImage: Option[Image] = None)