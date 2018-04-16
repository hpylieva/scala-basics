import scala.annotation.tailrec

object  sets{
  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): Set = (x) => x == elem

  def union(s: Set, t: Set): Set = (x) => contains(s, x) || contains(t, x)
  def intersect(s: Set, t: Set): Set = (x) => contains(s, x) && contains(t, x)
  def diff(s: Set, t: Set): Set = (x) => contains(s, x) && !contains(t, x)
  def filter(s: Set, p: Int => Boolean): Set = (x) => p(x) && contains(s, x)


  def forall(s: Set, p: Int => Boolean): Boolean ={
    @tailrec
    def iter(a: Int): Boolean ={
      if (a > 1000) true
        else if(contains(s,a) && !p(a)) false
        else iter(a+1)
    }
    iter(-1000)
  }

  def exists(s:Set, f: Int => Boolean): Boolean = !forall(s, x => !f(x))

  /*
   * the result of map is a new set which comprises elements {y} such that
   * f(y) is in initial set s
   */
  def map(s: Set, f: Int => Int): Set = (x) => exists(s, y => f(y) == x)



  val a = singletonSet(5)
  val b = singletonSet(10)
  val c = singletonSet(15)
  val d = singletonSet(7)
  val e = union(union(a, b),c)

  forall(e, x => x%5==0)
  forall(e, x => x%2==0)
  exists(e, x => x%3==0)
  exists(diff(e, c), x => x%3==0)

  val f = map(d, x=>x*2)
  forall(f, x => x%14==0)

}