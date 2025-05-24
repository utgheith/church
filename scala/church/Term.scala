// Term

import language.implicitConversions

/////////// Thunk //////////

case class Thunk[+A](f: () => A) {
  private lazy val the_val = f()

  def get: A = the_val
}

def thunk[A](a: => A): Thunk[A] = Thunk { () => a }

//////////////////////// Term ///////////////////////

enum Term {
  case App(lhs: Term, rhs: Term)
  case Abs(name: String, rhs: Term)
  case Ref(name: String)

  def apply(rhs: Term): App =
    App(this, rhs)
  def -:(lhs: String): Abs = Abs(lhs, this)
}

object Term {

  given Conversion[String, Ref] = s => Ref(s)
}

extension (s: String) {
  def apply(rhs: String): Term.App = Term.App(Term.Ref(s), Term.Ref(rhs))
  def apply(rhs: Term): Term.App = Term.App(Term.Ref(s), rhs)
  def -:(lhs: String): Term.Abs = Term.Abs(lhs, Term.Ref(s))
}

//////////////// Denote /////////////////

enum Denote[A] {
  case Error(term: Term, msg: String)
  case Pass(term: Term)
  case Func(term: Option[Term], func: A => Denote[A])
  case Value(v: A)

}

type Env[A] = Map[String, Thunk[Denote[A]]]

object Denote {
  given [A] => Conversion[A, Denote[A]] = a => Value(a)
  given [A] => Conversion[A => A, Denote[A]] = f => Func(None, a => Denote.Value(f(a)))

  def of[A](env: Env[A], t: Term): Thunk[Denote[A]] = thunk {
    t match {
      case Term.Abs(name, rhs) =>
        Func[A](
          Some(t),
          { a =>
            of[A](env.updated(name, thunk(Value(a))), rhs).get
          }
        )

      case Term.App(lhs, rhs) =>
        of(env, lhs).get match {
          case e: Error[A] => e
          case p: Pass[A] => p
          case Value(v) => Error(t, s"not sure how to apply $v to $rhs")
          case Func(_, f) =>
            of(env, rhs).get match {
              case Value(v) => f(v)
              case x => Error(t, s"not sure how to apply $lhs to $rhs = $x")
            }
        }
      case Term.Ref(name) =>
        env.getOrElse(name, thunk(Denote.Error(t, s"$name not found"))).get match {
          case Pass(t1) => of(env, t1).get
          case x => x
        }
    }
  }
}
