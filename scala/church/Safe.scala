import annotation.tailrec

// Safe recursion

sealed trait Safe[+A] {
  import Safe.{FlatMap, Value}
  def flatMap[B](f: A => Safe[B]): Safe[B] = FlatMap(this, f)
  def map[B](f: A => B): Safe[B] = flatMap(a => Safe(f(a)))

  @tailrec
  final def get: A = this match {
    case Value(a)                    => a
    case FlatMap(Value(x), bind)     => bind(x).get
    case FlatMap(FlatMap(s, b1), b2) => FlatMap(s, s => b1(s).flatMap(b2)).get
  }
}

object Safe {
  case class Value[A](a: A) extends Safe[A]
  case class FlatMap[A, B](sa: Safe[A], bind: A => Safe[B]) extends Safe[B]
  def apply[A](a: A): Safe[A] = Value(a)
}
