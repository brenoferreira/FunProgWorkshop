type Set = Int => Boolean
val bound = 1000
def contains(set:Set, n:Int) = set(n)

def toStr(s: Set): String = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.mkString("{", ",", "}")
}


def singleton(n:Int):Set =
  (x:Int) => x == n
contains(singleton(10), 10)

def union(set1: Set, set2: Set): Set =
  (x:Int) => set1(x) || set2(x)

contains(
  union(
    singleton(10), singleton(20)),
  10)
contains(
  union(
    singleton(10), singleton(20)),
  30)

def intersect(set1: Set, set2: Set): Set =
  (x:Int) => set1(x) && set2(x)

contains(
  intersect(
    union(
      singleton(10), singleton(20)),
    union(
      singleton(10), singleton(30))),
  10)

contains(
  intersect(
    union(
      singleton(10), singleton(20)),
    union(
      singleton(10), singleton(30))),
  20)

def diff(set1: Set, set2: Set): Set =
  (x:Int) => set1(x) && !set2(x)

contains(
  diff(
    union(
      singleton(10), singleton(20)),
    union(
      singleton(10), singleton(30))),
  20)

contains(
  diff(
    union(
      singleton(10), singleton(20)),
    union(
      singleton(10), singleton(30))),
  10)
def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > bound) true
    else if (a >= -bound && a <= bound && s(a))
      p(a) && iter(a + 1)
    else iter(a + 1)
  }
  iter(-bound - 1)
}
val set = union(singleton(10), singleton(20))
forall(set, (x:Int) => x % 10 == 0)
forall(set, (x:Int) => x < 10)

def exists(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > bound) false
    else if (a >= -bound && a <= bound && s(a))
    p(a) || iter(a + 1)
    else iter(a + 1)
  }
  iter(-bound - 1)
}
//x belongs to mapped set if there is a value in the Set s which will be equal to x after applying function f
def map(s: Set, f: Int => Int): Set =
  (x:Int) => exists(s, (n:Int) => f(n) == x)
exists(set, (x:Int) => x == 10)
toStr(set)
val mappedSet = map(set, (n:Int) => n + 1)
toStr(mappedSet)

