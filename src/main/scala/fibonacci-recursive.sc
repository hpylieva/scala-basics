object fibonacci {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  2+3                                             //> res0: Int(5) = 5

  def fib(n: Int): Int =
    if (n == 0 || n==1) n
    else fib(n-1) +fib(n-2)   //> fib: (n: Int)Int

  fib(0)                                    //> res1: Int = 0
  fib(1)                                    //> res2: Int = 1
  fib(2)                                    //> res3: Int = 1
  fib(3)                                    //> res4: Int = 2
  fib(4)                                    //> res5: Int = 3
  fib(5)                                    //> res6: Int = 5

}