package matrices

import scala.math.Numeric.Implicits._

class Matrix2D[N: Numeric](val data: Seq[Vector[N]], val rows: Int, val cols: Int) {

  val size = (rows, cols)

  // require col-number (size of each row) is constant, e.g. [1,2,3],[4,5,6]
  require(data.map(vec => vec.size).toSet.size == 1)

  // auxiliary constructor with only data parameter
  def this(data: Seq[Vector[N]]) = {
    this(data, data.size, data.map(vec => vec.size).toSet.head)
  }

  // show method
  def show = {
    for(vec <- data){
      println(s"[${vec.mkString(",")}]")
    }
  }

  // overloaded show method, with parameter
  def show(separator: String = ",") = {
    for(vec <- data){
      println(s"[${vec.mkString(separator)}]")
    }
  }

  def transpose: Matrix2D[N] = {
     val transposed = for(c <- 0 to this.cols-1) yield(this.data.map(v => v(c)).toVector)
    new Matrix2D(transposed)
  }

  // define matrix plus operator
  def + (that: Matrix2D[N]): Matrix2D[N] = {
    require(this.size == that.size)
    def plusVecElems[N : Numeric](a: Vector[N], b: Vector[N]): Vector[N] = {
      a zip b map { case(x: N,y: N) => x + y}
    }

    val zipped: Seq[(Vector[N],Vector[N])] = this.data zip that.data
    val n = zipped.map{ t => plusVecElems(t._1,t._2) }
    new Matrix2D(n)
  }

  // define matrix minus operator
  def - (that: Matrix2D[N]): Matrix2D[N] = {
    require(this.size == that.size)
    def minusVecElems[N: Numeric](a: Vector[N], b: Vector[N]): Vector[N] = {
      a zip b map { case (x: N, y: N) => x - y }
    }

    val zipped: Seq[(Vector[N], Vector[N])] = this.data zip that.data
    val n = zipped.map{ t => minusVecElems(t._1, t._2) }
    new Matrix2D(n)
  }

  // define matrix multiplication operator
  def * (that: Matrix2D[N]): Matrix2D[N] = {
    require(this.cols == that.rows)

    val zipped: Seq[(Vector[N], Vector[N])] = for(t1 <- this.data; t2 <- that.transpose.data) yield (t1,t2)
    val n: Seq[N] = zipped.map{ t => t._1.sum + t._2.sum }
    val m: Seq[Vector[N]] = n.sliding(this.rows,this.rows).toList.map(_.toVector)

    new Matrix2D(m)
  }

}

// companion object
object Matrix2D{
  // factory method: apply
  def apply[N: Numeric](data: Seq[Vector[N]]) = new Matrix2D[N](data)
}