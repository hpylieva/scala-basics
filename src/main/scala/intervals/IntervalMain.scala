
object IntervalMain{
  def main(args: Array[String]): Unit = {
    val i1 = new Interval(2.3, 5.6)
    println(i1)

    //    val i2 = new Interval(2.3, -5.6)

    val i3 = new Interval(2, 5)
    val i4 = new Interval(3, 9)
    println( i3 + i4 )
    println( i3.+(i4) )

    println(-i3)
    println(i3 intersect i4)
    println(i3 union i4)

    println(i4 intersect i3)
    println(i4 union i3)

    val i5 = new Interval(7, 12)
    println( (i3 union i4) union i5 )
    println( i3 union (i4 union i5) )
  }
}
