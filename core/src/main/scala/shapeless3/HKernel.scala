package shapeless3

import scala.compiletime.ops.int.*

trait HKernel:
  type L <: Tuple
  type Length <: Int
  def length: Int

trait EmptyTupleHKernel extends HKernel:
  type L = EmptyTuple
  type Length = 0
  def length: Int = 0

case object EmptyTupleKernel extends EmptyTupleHKernel

final case class HConsHKernel[H, T <: HKernel](tail: T) extends HKernel:
  type L = H *: tail.L
  type Length = S[tail.Length]
  def length: Int = 1 + tail.length

object HKernel:
  def apply[L <: Tuple](implicit mk: HKernelAux[L]): mk.Out = mk()
  def apply[L <: Tuple](l: L)(implicit mk: HKernelAux[L]): mk.Out = mk()

trait HKernelAux[L <: Tuple] extends DepFn0 { type Out <: HKernel }

object HKernelAux:
  type Aux[L <: Tuple, Out0 <: HKernel] = HKernelAux[L] { type Out = Out0 }

  implicit val mkHNilHKernel: HKernelAux[EmptyTuple] = new HKernelAux[EmptyTuple]:
    type Out = EmptyTupleKernel.type
    def apply() = EmptyTupleKernel

  implicit def mkHListHKernel[H, T <: Tuple, CtOut <: HKernel](implicit ct: HKernelAux.Aux[T, CtOut]): HKernelAux[H *: T] =
    new HKernelAux[H *: T] {
      type Out = HConsHKernel[H, CtOut]
      def apply() = HConsHKernel[H, CtOut](ct())
    }