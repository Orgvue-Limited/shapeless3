package shapeless3

trait PolyApply:
  type λ <: Singleton

  def apply[A](a: A)(using cse: PolyCases.Case[λ, A *: EmptyTuple]): cse.Result = cse(a *: EmptyTuple)

  def apply[A, B](a: A, b: B)(using cse: PolyCases.Case[λ, A *: B *: EmptyTuple]): cse.Result = cse(a *: b *: EmptyTuple)

trait Poly extends PolyApply:
  type λ = this.type

  type ProductCase[L <: Tuple] = PolyCases.Case[this.type, L]

  object ProductCase extends Serializable:
    type Aux[L <: Tuple, Result0] = ProductCase[L] {type Result = Result0}

    def apply[L <: Tuple, R](v: L => R) = new ProductCase[L]:
      type Result = R
      val value = v

  def apply[R](using c: ProductCase.Aux[EmptyTuple, R]): R = c()

trait PolyInst:
  implicit def inst0(p: Poly)(implicit cse: p.ProductCase[EmptyTuple]): cse.Result                             = cse()
  implicit def inst1[A](fn: Poly)(implicit cse: fn.ProductCase[A *: EmptyTuple]): A => cse.Result              =
    a => cse(a *: EmptyTuple)
  implicit def inst2[A, B](fn: Poly)(implicit cse: fn.ProductCase[A *: B *: EmptyTuple]): (A, B) => cse.Result =
    (a, b) => cse(a *: b *: EmptyTuple)

object Poly extends PolyInst

trait Poly0 extends Poly:
  type Case0[T] = ProductCase.Aux[EmptyTuple, T]

  def at[T](t: T) = new ProductCase[EmptyTuple]:
    type Result = T
    val value = _ => t

trait Poly1 extends Poly { self =>
  type Case[A] = PolyCases.Case[self.type, A *: EmptyTuple]

  object Case:
    type Aux[A, Result0] = PolyCases.Case.Aux[self.type, A *: EmptyTuple, Result0]

  class CaseBuilder1[A]:
    def apply[Res](fn: A => Res): Case.Aux[A, Res] = PolyCases.Case { case a *: EmptyTuple => fn(a) }

  def at[A]: CaseBuilder1[A] = new CaseBuilder1[A]
}

trait Poly2 extends Poly { self =>
  type Case[A, B] = PolyCases.Case[self.type, A *: B *: EmptyTuple]

  object Case:
    type Aux[A, B, Result0] = PolyCases.Case.Aux[self.type, A *: B *: EmptyTuple, Result0]

  class CaseBuilder2[A, B]:
    def apply[Res](fn: (A, B) => Res): Case.Aux[A, B, Res] = PolyCases.Case { case a *: b *: EmptyTuple => fn(a, b) }

  def at[A, B]: CaseBuilder2[A, B] = new CaseBuilder2[A, B]
}
