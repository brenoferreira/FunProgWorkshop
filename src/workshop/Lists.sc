
trait List[T] {
  def isEmpty:Boolean
  def head:T
  def tail:List[T]
}

case class Empty[T] extends List[T] {
  def isEmpty = true
  def head = throw new Exception
  def tail = throw new Exception
}


case class Cons[T] (val head:T, val tail:List[T]) extends List[T]{
  def isEmpty = false
}


def fold[A, B](xs:List[A], initial:B, f:(B, A) => B):B ={
  xs match{
    case Empty() => initial
    case Cons(h, t) => fold(t, f(initial, h), f)
  }
}

def concat[A](xs:List[A], ys:List[A]):List[A] = {
  xs match {
    case Empty() => ys
    case Cons(h, t) => Cons(h, concat(t, ys))
  }
}


def flatMap[A, B](xs:List[A], f:A => List[B]):List[B] = {
  xs match {
    case Empty() => Empty()
    case Cons(h, t) => f(h) match {
      case Empty() => flatMap(t, f)
      case Cons(h2, t2) =>
        Cons(h2, concat(t2, flatMap(t, f)))
    }
  }
}

def map[A, B](xs:List[A], f:A => B):List[B] = {
  flatMap(xs, (x:A) => Cons(f(x), Empty()))
}

def filter[A](xs:List[A], f:A => Boolean):List[A] = {
  flatMap(xs, (x:A) => f(x) match{
    case true => Cons(x, Empty())
    case _ => Empty()
  })
}
val list1 = Cons(1, Cons(2, Cons(3, Empty())))
val list2 = Cons(4, Cons(5, Cons(6, Empty())))
map(list1, (x:Int) => x * x)
filter(list2, (x:Int) => x > 4)

