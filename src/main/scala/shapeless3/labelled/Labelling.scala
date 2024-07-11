package shapeless3.labelled

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import shapeless3.DepFn0

trait Labelling[T] extends DepFn0 with Serializable:
  type Out <: Tuple

object Labelling:
  type Aux[T, Out0] = Labelling[T] { type Out = Out0 }

  def apply[T](implicit lab: Labelling[T]): Aux[T, lab.Out] = lab

  inline given [T](using m: Mirror.Of[T]): Labelling.Aux[T, m.MirroredElemLabels] =
    val tuple = constValueTuple[m.MirroredElemLabels]

    new Labelling[T]:
      override type Out = m.MirroredElemLabels
      override def apply(): Out = tuple