package shapeless3

trait Cases:
  type Case1[Fn, A] = PolyCases.Case[Fn, A *: EmptyTuple]

  object Case1:
    type Aux[Fn, A, Result] = PolyCases.Case.Aux[Fn, A *: EmptyTuple, Result]
    def apply[Fn, A, Result](fn: A => Result): Case1.Aux[Fn, A, Result] =
      PolyCases.Case { case a *: EmptyTuple => fn(a) }

  type Case2[Fn, A, B] = PolyCases.Case[Fn, A *: B *: EmptyTuple]

  object Case2:
    type Aux[Fn, A, B, Result] = PolyCases.Case.Aux[Fn, A *: B *: EmptyTuple, Result]

    def apply[Fn, A, B, Result](fn: (A, B) => Result): Case2.Aux[Fn, A, B, Result] =
      PolyCases.Case { case a *: b *: EmptyTuple => fn(a, b) }

trait CaseInst:
  given inst1[Fn <: Poly, A, Res]: Conversion[PolyCases.Case.Aux[Fn, A *: EmptyTuple, Res], A => Res] =
    cse => a => cse.value(a *: EmptyTuple)
  given inst2[Fn <: Poly, A, B, Res]: Conversion[PolyCases.Case.Aux[Fn, A *: B *: EmptyTuple, Res], (A, B) => Res] =
    cse => (a, b) => cse.value(a *: b *: EmptyTuple)

object PolyCases extends Cases:
  trait Case[P, L <: Tuple]:
    type Result
    val value: L => Result

    def apply(t: L): Result = value(t)
    def apply()(using ev: EmptyTuple =:= L): Result = value(EmptyTuple)
    def apply[T](t: T)(using ev: (T *: EmptyTuple) =:= L): Result = value(t *: EmptyTuple)
    def apply[T, U](t: T, u: U)(using ev: (T *: U *: EmptyTuple) =:= L): Result = value(t *: u *: EmptyTuple)

  object Case extends CaseInst:
    type Aux[P, L <: Tuple, Result0] = Case[P, L] {type Result = Result0}

    def apply[P, L <: Tuple, R](v: L => R): Aux[P, L, R] = new Case[P, L]:
      type Result = R
      val value = v