package matrices

import org.scalatest.funsuite.AnyFunSuite

class Matrix2DTest extends AnyFunSuite {

  val v1 = Vector(1,2,3,4)
  val v2 = Vector(5,6,7,8)
  val v3 = Vector(0,7,1,6)
  val m1 = new Matrix2D[Int](Seq(v1,v2,v3))


  test("Transposed matrix will switch count of rows to count cols ") {
    assert(m1.transpose.rows == m1.cols)
  }
}
