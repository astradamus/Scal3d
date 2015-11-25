package com.alexanderstrada.graphics.face_renderer

/**
 * Renders a given sequence of faces.
 */
object FaceRenderer {

  def apply(canvas: FaceCanvas, faces: Seq[Face]): Unit = faces.foreach(f => renderFace(canvas, f))


  def renderFace(canvas: FaceCanvas, face: Face): Unit = {

    face.fill foreach (fillColor => {
      canvas.setColor(fillColor)
      canvas.fill(face.rectangle)
    })

    face.outline foreach (outlineColor => {
      canvas.setColor(outlineColor)
      canvas.outline(face.rectangle)
    })

    face.image foreach (img => {
      canvas.drawImage(img, face.rectangle)
    })
  }
}
