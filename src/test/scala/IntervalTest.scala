
import org.scalatest.FunSuite

class IntervalTest extends FunSuite {

  test("Interval boudaries are correct") {
    val i1 = new Interval(2, 3)
    assert(i1.lowerBound == 2)
    assert(i1.upperBound == 3)
  }

  test("Interval boudaries coul not be reverted") {
    intercept[IllegalArgumentException] {
      new Interval(3, 2)
    }
  }

  test("Union of two intervals is an interval with min of lowerBounds " +
    "and max of upperBounds") {
    val union1 = new Interval(2, 5) union new Interval(4, 7)
    assert(union1.lowerBound == 2)
    assert(union1.upperBound == 7)
  }

}