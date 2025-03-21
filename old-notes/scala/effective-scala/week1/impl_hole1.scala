package ImplHole

def which(n: Int): String =
  if n >= 0 then
    "Positive"
  else
    ???

@main def main(): Unit =
  println(which(1))
  println(which(-1))
