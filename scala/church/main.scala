import language.implicitConversions

val x = "x"
val f = "f"
val z = "z"
val w = "w"

val zero: Term = f -: x -: x
val one: Term = f -: x -: f(x)
val inf: Term = (x -: x(x))(x -: x(x))

val it: Term = (x -: z)((w -: w(w(w)))(w -: w(w(w))))

@main
def main(): Unit =
  println("Hello world!")
  pprint.pprintln(zero)
  pprint.pprintln(one)
  pprint.pprintln(inf)

  pprint.pprintln(it)

  pprint.pprintln(Denote.of[Int](Map(
    "f" -> thunk { (x: Int) => x + 1 },
    "x" -> thunk { 0 }
  ), (f -: f(x))(f)).get)
