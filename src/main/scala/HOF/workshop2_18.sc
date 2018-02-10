import sun.nio.cs.ext.DoubleByteEncoder

object workshop2_18{

  def sumOfFuncs(f: Double=>Double, g: Double => Double): Double => Double ={
    def sum(x:Double): Double= f(x) + g(x)
    sum
  }

  def sumOfFuncs2(
                 f: Double => Double,
                 g: Double => Double): Double => Double = {
    (x) => g(x) + f(x)
  }
  // the second variant is good for somr inner functions, not seen
  // for outer world

  /* difference between dev and val:
 * we are evaluating def each time
 * val is evaluated only once
 */

  def func1(x: Double) = x + 2
  val func2 = (x:Double) => x*2

  val sum1 = sumOfFuncs2(func1, func2)
  sum1
  sum1(3+4)

  def sumOfFuncs3( f: Double => Double):
  (Double => Double) => Double => Double = {
    def g =  Math.sqrt(5) // this function doesn't impact on result of sumOfFuncs3
    (g: Double => Double) => //here we could have witten just (g) => (x) =>
    (x) => g(x) + f(x)
  }

  sumOfFuncs3(x => x+2)(x => x*2)(3+4)

  def sumOfInts0(x:Int, y:Int):Int = x+y

  def sumOfInts1(x:Int): Int => Int= {
    y => x + y
  }

  //demonstration of functions overloading
  def sumOfInts2(x:Int)(y:Int):Double = x+y+2.1
 // def sumOfInts2(x:Int)(y:Double):Double = x+y

  //demonstration of underscore
  // underescore means that we are returning a function with inspecified all the rest of arguements
  // so some of args are defined and all the rest are expected later
  sumOfInts2(1)(2)
  def f=sumOfInts2(1)_
  f(2)

  //-----------Types-----------

  type Vector = Int => Double
  type Matrix = (Int, Int) => Double

  val matrix1: Matrix = (row, column) => if (row==column) 1.0 else 0.0

  def getRow(m: Matrix, row:Int): Vector = column => m(row, column)
  def getColumn(m: Matrix, column:Int): Vector = row => m(row, column)

  val row4 = getRow(matrix1, 4)

  val vector1: Vector = index => index +1
  vector1(6)
  vector1(9)

  val vector2: Vector = index => index
  sumOfFuncs3(x => x + 3)(x => x * 8)

  //next task - to multiply matrices
  def mult(m1: Matrix, m2: Matrix, vectorMult: (Vector, Vector) => Double): Matrix =
    (resultRow, resultColumn) =>
      vectorMult(getRow(m1, resultRow), getColumn(m1, resultColumn))


}
