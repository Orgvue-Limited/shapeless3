package shapeless3

import shapeless3.macros.CoproductMacro
import shapeless3.labelled.LabelledType

import scala.deriving.Mirror

trait LabelledGeneric[T] extends Serializable:
  type Repr
  def to(t : T) : Repr
  def from(r : Repr) : T

object LabelledGeneric:
  type Aux[T, Repr0] = LabelledGeneric[T]{ type Repr = Repr0 }

  def apply[T](implicit lgen: LabelledGeneric[T]): Aux[T, lgen.Repr] = lgen

  def instance[T, R](f: T => R, g: R => T): Aux[T, R] =
    new LabelledGeneric[T]:
      type Repr = R
      def to(t: T): R = f(t)
      def from(r: R): T = g(r)

  object ops:
    extension[A] (a: A)
      def toRepr(using g: LabelledGeneric[A]): g.Repr = g.to(a)

    extension[Repr] (a: Repr)
      def to[A](using g: LabelledGeneric.Aux[A, Repr]): A = g.from(a)

  inline implicit def deriveProduct[T <: Product](using
    m: Mirror.ProductOf[T],
    l: LabelledType[T],
    g: Generic[T]
  ): Aux[T, l.Out] =
    instance(l(_), repr => g.from(repr.asInstanceOf[g.Repr]))

  transparent inline implicit def deriveCoproduct[T]: LabelledGeneric.Aux[T, ?] =
    ${ CoproductMacro.deriveCoproductForLabelledGeneric[T] }
