import matrices.Matrix2D

object Main {

  def main(args: Array[String]): Unit = {

    val v1 = Vector(1,2,3,4)
    val v2 = Vector(5,6,7,8)
    val v3 = Vector(0,7,1,6)

    val m1 = new Matrix2D[Int](Seq(v1,v2,v3))
    val m2 = new Matrix2D[Int](Seq(v3,v1,v1))

    println("m1")
    m1.show

    println("m2")
    m2.show

    println("m2.transpose")
    m2.transpose.show

    println("m3")
    val m3 = m1 + m2
    m3.show
  }

}
