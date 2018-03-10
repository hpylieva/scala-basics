

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

/**
  * ScalaCheck property-based test suite.
  */
object Interval1Specification extends Properties("Interval") {
lazy val genInterval: Gen[Interval] = for {
    k <- arbitrary[Double].suchThat(x => x < 1000 || x > -1000)
    m <- arbitrary[Double].suchThat(y => y > k && (y < 1000 || y > -1000))
  } yield new Interval(k, m)

  implicit lazy val arbInterval: Arbitrary[Interval] = Arbitrary(genInterval)

  val intersectedIntervals: Gen[(Interval, Interval)] =
    for {
      i1 <- arbitrary[Interval]
      i2 <- arbitrary[Interval].filter(i =>
        math.max(i.lowerBound, i1.lowerBound)
          < math.min(i.upperBound, i1.upperBound))
    } yield (i1, i2)

  property("union commutativity") = forAll(intersectedIntervals) { pair =>
    (pair._1 union pair._2) == (pair._2 union pair._1)

  }

}
