

object monoids{

  trait Monoid[A]{
    def op(x: A, y: A): A
    def zero: A
  }

  def stringMonoid: Monoid[String] = new Monoid[String] {
    def op(x: String, y: String) = x + y
    def zero  = ""
  }

  def listMonoid[A] = new Monoid[List[A]]{
    override def op(x: List[A], y: List[A]) =  x++y
    override def zero = Nil
  }

  def concat[A](xs: List[A], m: Monoid[A]): A =
    xs.foldLeft(m.zero)(m.op)

  def foldMap[A, B](xs: List[A], m:Monoid[B])(f:A=>B): B =
    xs.foldLeft(m.zero)((b,a) => m.op(b, f(a)))

//  indexedSeq == enumerate in Python
  def foldMapBalanced [A, B] ( xs : IndexedSeq [A] , m: Monoid [B] )
                             (f: A => B): B =
    if (xs.isEmpty)
      m.zero
    else if (xs.length ==1)
      f(xs(0))
    else {
      val (l,r) = xs.splitAt(xs.length /2)
      m.op(foldMapBalanced(l, m)(f),
           foldMapBalanced(r,m)(f))
    }

  /**
    * Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll
    * need to come up with a creative Monoid.
    */
  def seqIsOrdered(ints: IndexedSeq[Int]): Boolean = {
    // Reference implementation
    // (Int, Int, Boolean) = (Min, Max, IsOrdered)
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]):
                  Option[(Int, Int, Boolean)] =
        (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero: None.type = None
    }
    foldMapBalanced(ints, mon)(i => Some((i, i, true))).forall(_._3)
  }

  def main(args: Array[String]): Unit = {
    val words = List( "Hello", "Monoidal", "Folding")
    val rightSequence = words.foldRight(stringMonoid.zero)(stringMonoid.op)
    println(rightSequence)

  }
}
