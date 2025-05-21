from typing import cast, Callable


#
# Church encoding of pairs
#

type Selector[A,B] = Callable[[A,B], A|B]
type Pair[A,B] = Callable[[Selector[A,B]],A|B]

def pair[A,B](a: A, b: B) -> Pair[A,B]:
    return lambda f: f(a,b)

def fst[A,B](p: Pair[A,B]) -> A:
    return cast(A, p(lambda a, _: a))

def snd[A,B](p: Pair[A,B]) -> B:
    return cast(B, p(lambda _, b: b))

#
# Church encoding of booleans
#

type Boolean[A,B] = Callable[[A, B], A|B]

def true[A](a: A, _: object) -> A:
    return a

def false[B](_: object, b: B) -> B:
    return b

def church_if[A,B](cond: Boolean[A,B], a: A, b: B) -> A|B:
    return cond(a,b)

def church_not[A,B](cond: Boolean[A,B]) -> Boolean[B,A]:
    return lambda a, b: cond(b, a)

def church_and[A,B](a: Boolean[A|B,B], b: Boolean[A,B]) -> Boolean[A,B]:
    return lambda x, y: a(b(x, y), y)