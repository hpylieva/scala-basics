package lists_and_maps

object listOperations{

  sealed trait List[+A] {
    def ++[B >: A](ys: List[B]): List[B] = this match {
      case Nil => ys
      case Cons(head, tail) => Cons(head, tail ++ ys)
    }
  }

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    override def toString: String = head + "\u279D" + tail
  }

  def concat[A](xs: List[A], ys: List[A]): List[A] =
    xs match {
      case Nil => ys
      case Cons(h, t) => Cons(h, concat(t, ys))
    }

  def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] =
    xs match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => xs
    }

  def foldRight[A, B](xs: List[A])(zero: B)(f: (A, B) => B): B = xs match {
    case Nil => zero
    case Cons(h, t) => f(h, foldRight(t)(zero)(f))
  }

  def foldLeft[A, B](xs: List[A])( zero: B)(f: (B, A) => B): B = xs match {
    case Nil => zero
    case Cons(h, t) => foldLeft(t)(f(zero,h))(f)
  }

  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs)(Nil:List[A])((acc,h) => Cons(h,acc))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def map[A, B](xs: List[A])(f: A => B): List[B] = xs match {
    case Nil =>
      Nil
    case Cons(y, ys) =>
      Cons(f(y), map(ys)(f))
  }

  def main(args: Array[String]): Unit = {
    val list1 = Cons("Hello", Cons("World", Cons("!", Nil)))
    println(list1)
    println(foldRight(list1)(0)((str, combined) => str.length() + combined))
//    println(foldRight(Cons(1, Cons(2, Cons(3, Nil))),0)(_+_))
    println(concat(Cons(1, Cons(2, Nil)), Cons(3, Nil)))
    println(reverse(list1))
  }
}
