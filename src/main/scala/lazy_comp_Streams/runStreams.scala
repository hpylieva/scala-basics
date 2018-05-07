object runStreams {
  def main(args: Array[String]): Unit = {
    val stream = Stream(1, 2, 3, 4, 5)
    println(stream) //Cons(<function0>,<function0>)
    println(stream.toList) //List(1, 2, 3)
    println(stream.take(3).toList)
    println(stream.drop(2).toList)

    val s1 = Stream(3, 4, 8)
    println(s1.exists(x => x % 3 == 0))
    println(s1.forAll(x => x % 2 == 0))

    val more = Stream.from(100)
    println(more.take(5).toList) //List(100, 101, 102, 103, 104)

    val fibs = {
      def next(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, next(f1, f0 + f1))
      next(0, 1)
    }
    println(fibs.take(10).toList) //List(0, 1, 1, 2, 3, 5, 8)

    val s2 = Stream(1, 2, 3, 4, 5)
    println(s2.map(x => x * 3).toList)
    println(s2.filter(x => x % 2 == 0).toList)

    val primes = {
      def isPrime(n: Int, i: Int = 2): Boolean = {
//        n match {
//          case 0|1 => false
//          case 2|3 => true
//          case _  if (n % i != 0) && (i * i > n) => true
//          case _ => isPrime(n, i + 1)
//          case _ =>
        if (n <= 2) return if (n == 2) true
          else false
        if (n % i == 0) return false
        if (i * i > n) return true
        // Check for next divisor
        isPrime(n, i + 1)
      }
      def next(p0: Int): Stream[Int] = Stream.cons(p0, next(p0+1).filter(x=>isPrime(x)))
      next(1)
    }

    println(primes.take(10).toList)


  }
}
