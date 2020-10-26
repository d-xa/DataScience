import matrices.Matrix2D

object Main {

  def main(args: Array[String]): Unit = {

    val v1 = Vector(1,2,3,4)
    val v2 = Vector(5,6,7,8)
    val v3 = Vector(0,7,1,6)

    val m1 = Matrix2D(Seq(v1,v2,v3)) // using apply-method, compiler syntactic sugar
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

    val v4 = Vector(3,5,1)
    val v5 = Vector(1,4,0)
    val v6 = Vector(1,1,3)
    val v7 = Vector(2,1,1)
    val m4 = Matrix2D(Seq(v4,v5,v6,v7))

    println("m4")
    m4.show
    println("m4-transpose")
    m4.transpose.show

    println("matrix mul")
    val m5 = m1 * m4
    m5.show

    println(m5.getCol(0))
    println(m5.getCol(1))
    println(m5.getCol(2))

  }

}
