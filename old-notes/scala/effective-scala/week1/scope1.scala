package Scope1

var a = 1

def pa(): Unit =
  println(a)

def sqr(x: Int): Int =
  val y =
    val z = x * x
    z
  y

@main def main(): Unit =
  pa()
  println(sqr(x = 4))
