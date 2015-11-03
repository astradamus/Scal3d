/**
 *
 */

import com.alexanderstrada.space3d.Box
import com.alexanderstrada.space3d.tuple3d.{Tuple3dD, Point3dD, Size3dD}
import org.scalatest._

class test_Box extends FlatSpec with Matchers {

  val box1 = Box(Point3dD(0, 0, 0),
                 Size3dD(100, 100, 100))

  val box2 = Box(Point3dD(100, 100, 100),
                 Size3dD(100, 100, 100))

  val box3 = Box(Point3dD(50, 50, 50),
                 Size3dD(25, 25, 25))

  val box4 = Box(Point3dD(-250, -250, -250),
                 Size3dD(500, 500, 500))


  "box1" should "intersect box2, but not exclusively" in {
    box1.intersects(box2) should be (right = true)
    box1.exclusiveIntersects(box2) should be (right = false)
  }

  it should "contain box3, but not box2" in {
    box1.contains(box3) should be (right = true)
    box1.contains(box2) should be (right = false)
  }

  it should "produce a proper union with box2" in {
    box1.union(box2) should be (Box(Point3dD(0, 0, 0), Size3dD(200, 200, 200)))
  }

  it should "not change when unioned with a box it already contains" in {
    box1.union(box3) should be (box1)
  }

  "box.compare(box)" should "produce correct comparisons" in {
    box4.compareTo(box1) should be (Tuple3dD(-250, -250, -250))
    box4.compareTo(box2) should be (Tuple3dD(-150, -150, -150))
    box4.compareTo(box3) should be (Tuple3dD(-200, -200, -200))
    box2.compareTo(box1) should be (Tuple3dD.ZERO)
    box3.compareTo(box4) should be (Tuple3dD(200, 200, 200))
  }

}
