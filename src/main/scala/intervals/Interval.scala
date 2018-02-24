
class Interval(left: Double, right: Double){
  require(left < right,
    s"Lower bound $left is greater than upper bound $right")

  def lowerBound = left
  def upperBound = right

  override def toString =
    "[" + lowerBound + "; " + upperBound + ")"

  def +(that: Interval): Interval =
    new Interval(
      this.lowerBound + that.lowerBound,
      this.upperBound + that.upperBound
    )

  def unary_- : Interval =
    new Interval(-upperBound, -lowerBound)

  def *(that: Interval): Interval = {
    def min(a: Double, b: Double, c: Double, d: Double) = {
      val min1 = math.min(a, b)
      val min2 = math.min(min1, c)
      math.min(min2, d)
    }
    def max(a: Double, b: Double, c: Double, d: Double) = {
      val max1 = math.max(a, b)
      val max2 = math.max(max1, c)
      math.max(max2, d)
    }
    val p1 = this.lowerBound * that.lowerBound
    val p2 = this.lowerBound * that.upperBound
    val p3 = this.upperBound * that.lowerBound
    val p4 = this.upperBound * that.upperBound

    new Interval(min(p1, p2, p3, p4), max(p1, p2, p3, p4))
  }

  def /(that: Interval): Interval =
    this * new Interval(1.0 / that.upperBound, 1.0 / that.lowerBound)

  // only for intersected intervals
  def union(that: Interval): Interval =
    new Interval(
      math.min(this.lowerBound, that.lowerBound),
      math.max(this.upperBound, that.upperBound)
    )
  def intersect(that: Interval): Interval =
    new Interval(
      math.max(this.lowerBound, that.lowerBound),
      math.min(this.upperBound, that.upperBound)
    )

  // ∪ ∩

  // lowerBound < upperBound
  // -lowerBound > -upperBound

}

