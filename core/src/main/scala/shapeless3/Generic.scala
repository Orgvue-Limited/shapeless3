package shapeless3

import scala.deriving.Mirror

import shapeless3.macros.CoproductMacro

trait Generic[T] extends Serializable:
  type Repr
  def to(t: T): Repr
  def from(r: Repr): T

object Generic:
  type Aux[T, Repr0] = Generic[T] {type Repr = Repr0}

  def apply[T](implicit gen: Generic[T]): Aux[T, gen.Repr] = gen

  def instance[T, R](f: T => R, g: R => T): Aux[T, R] =
    new Generic[T]:
      type Repr = R

      def to(t: T): R = f(t)

      def from(r: R): T = g(r)

  object ops:
    extension [A](a: A)
      def toRepr(using g: Generic[A]): g.Repr = g.to(a)

    extension [Repr](a: Repr)
      def to[A](using g: Generic.Aux[A, Repr]): A = g.from(a)

  transparent inline implicit def deriveProduct[T <: Product](using
    m: Mirror.ProductOf[T],
    m1: Mirror.ProductOf[m.MirroredElemTypes]
  ): Generic[T] =
    instance(m1.fromProduct(_), m.fromProduct(_))

  transparent inline implicit def deriveCoproduct[T](using m: Mirror.SumOf[T]): Generic[T] =
    ${ CoproductMacro.deriveCoproductForGeneric[T] }
