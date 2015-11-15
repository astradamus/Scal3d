package com.alexanderstrada.space3d.tuple3d

import com.alexanderstrada.space3d.Axis._

class Tuple3d[A](val x: A,
                 val y: A,
                 val z: A) {

  def apply(axis: Axis): A = axis match {
    case X => x
    case Y => y
    case Z => z
  }

  def map[B](f: (A) => B) = new Tuple3d(f(x), f(y), f(z))
  def map[B](f: (Axis, A) => B) = new Tuple3d(f(X, x), f(Y, y), f(Z, z))

  def foreach[U](f: (A) => U): Unit = map(f)
  def foreach[U](f: (Axis, A) => U): Unit = map(f)

  def exists(f: (A) => Boolean): Boolean = f(x) || f(y) || f(z)
  def exists(f: (Axis, A) => Boolean): Boolean = f(X, x) || f(Y, y) || f(Z, z)

  def forall(f: (A) => Boolean): Boolean = f(x) && f(y) && f(z)
  def forall(f: (Axis, A) => Boolean): Boolean = f(X, x) && f(Y, y) && f(Z, z)

  def toSeq = Seq(x, y, z)
}

object Tuple3d {
  def apply[A](x: A, y: A, z: A) = new Tuple3d(x, y, z)

  def apply[A](map: Map[Axis, A], default: A) = new Tuple3d(map.getOrElse(X, default),
                                                            map.getOrElse(Y, default),
                                                            map.getOrElse(Z, default))
}