object fibonacci{

  def fib(n: Int): Int =
  {
    @annotation.tailrec
    def fibIter(left: Int, right: Int, counter: Int): Int =
      if (n == counter) left
      else fibIter(left + right, left, counter +1)
    fibIter(0, 1, 0) //going from the buttom to the top
  }

  fib(0)
  fib(1)
  fib(2)
  fib(3)
  fib(4)
  fib(5)

  def func(x: Int, y: => /*CBN param*/ Int) =
    if(x>y) x else y

  func(2+3, 3+4)
  func(5,3+4)
  if(5>(3+4)) 5 else (3+5)

  val  y0 = 9
  func(5, y0)

}