package shapeless3.tuples

import shapeless3.DepFn2
import shapeless3.PolyCases.Case2

trait RightFolder[L <: Tuple, In, HF] extends DepFn2[L, In] with Serializable

object RightFolder:
  def apply[L <: Tuple, In, F](using folder: RightFolder[L, In, F]): Aux[L, In, F, folder.Out] = folder

  type Aux[L <: Tuple, In, HF, Out0] = RightFolder[L, In, HF] { type Out = Out0 }

  given emptyTupleRightFolder[In, HF]: Aux[EmptyTuple, In, HF, In] =
    new RightFolder[EmptyTuple, In, HF]:
      type Out = In
      def apply(l : EmptyTuple, in : In): Out = in

  given consRightFolder[H, T <: Tuple, In, HF, OutT](using
    ft: RightFolder.Aux[T, In, HF, OutT],
    f: Case2[HF, H, OutT]
  ): Aux[H *: T, In, HF, f.Result] =
    new RightFolder[H *: T, In, HF]:
      type Out = f.Result
      def apply(l : H *: T, in : In): Out = f(l.head, ft(l.tail, in))