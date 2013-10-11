import scala.annotation.tailrec

def area(width:Double, height:Double) = width * height

def power (exp:Double, x:Double):Double =
  if(exp == 1) x
  else power(exp -1, x * x)

def square(x:Double) = power(2, x)
square(10)
val PI = 3.14159265359
def area(radius:Double) = PI * square(radius)
area(2)


def abs(x:Double) =
  if(x >= 0) x
  else -x

def sqrt(x:Int):Double = {
  def isGoodEnough(guess:Double) =
    abs(guess * guess - x) < 0.001

  def improve(guess:Double) =
    (guess + x / guess) / 2
  def sqrtImpl(guess:Double):Double =
    if(isGoodEnough(guess)) guess
    else sqrtImpl(improve(guess))
  sqrtImpl(1)
}
sqrt(100)
def factorial(x:Int):Int =
  if(x == 0) 1
  else x * factorial(x -1)

factorial(5)

def factorial2(x:Int) = {
  @tailrec
  def factorialImpl(x:Int, res:Int):Int =
    if(x == 0) res
    else factorialImpl(x-1, x*res)
  factorialImpl(x, 1)
}

factorial2(5)


