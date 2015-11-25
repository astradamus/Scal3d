package com.alexanderstrada.graphics.face_renderer

/**
 * Sorts a seq of faces by their sort depth and then renders them from back to front.
 */
object FaceRenderer {

  def apply(canvas: FaceCanvas, faces: Seq[Face]): Unit = faces
                                                            .toSeq
                                                            .sortBy(_.sortDepth)
                                                            .foreach(f => renderFace(canvas, f))


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
