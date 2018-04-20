import Stream.{cons, empty}

sealed trait Stream[+A]{
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toListRecursive: List[A] = this match {
    case Cons(h,t) => h() :: t().toListRecursive
    case _ => List()
  }

  def toList: List[A] = {
    @annotation.tailrec
    def move(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => move(t(), h() :: acc)
      case _=> acc
    }
    move(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n  -  1))
    case Cons(h, _ ) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match{
    case Cons(h, t) if n > 1 => t().drop(n  -  1)
    case Cons(h, t) if n == 1 => t()
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def forAll(p: A => Boolean): Boolean = this match{
    case Cons(h, t) if !p(h()) => false
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def foldRight[B](z: =>B)(f: (A, =>B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def map[B](f: A => B): Stream[B] = this match {
    case Cons(h, t) => cons(f(h()), t().map(f))
    case _ => empty
  }

  def filter(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().filter(p))
    case Cons(h, t ) if !p(h()) => t().filter(p)
    case _ => empty
  }
 // def filter

}
case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](args: A*): Stream[A] =
    if (args.isEmpty) empty
    else cons(args.head, apply(args.tail: _*))

  def from(n: Int): Stream[Int] =  cons(n, from(n+1))
}
