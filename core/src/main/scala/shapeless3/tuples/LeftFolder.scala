package shapeless3.tuples

import shapeless3.DepFn2
import shapeless3.PolyCases.Case2

trait LeftFolder[L <: Tuple, In, HF] extends DepFn2[L, In] with Serializable

object LeftFolder:
  def apply[L <: Tuple, In, F](using folder: LeftFolder[L, In, F]): Aux[L, In, F, folder.Out] = folder

  type Aux[L <: Tuple, In, HF, Out0] = LeftFolder[L, In, HF] { type Out = Out0 }

  given emptyTupleLeftFolder[In, HF]: Aux[EmptyTuple, In , HF, In] =
    new LeftFolder[EmptyTuple, In, HF]:
      type Out = In
      def apply(l : EmptyTuple, in : In): Out = in

  given consLeftFolder[H, T <: Tuple, In, HF, OutH, FtOut](using
    f: Case2.Aux[HF, In, H, OutH],
    ft : LeftFolder.Aux[T, OutH, HF, FtOut]
  ): Aux[H *: T, In, HF, FtOut] =
    new LeftFolder[H *: T, In, HF]:
      type Out = FtOut
      def apply(l : H *: T, in : In) : Out = ft(l.tail, f(in, l.head))