package shapeless3

package object tuples:
  extension [T <: Tuple] (tuple: T)
    def foldLeft[R](z : R)(op : Poly)(using folder: LeftFolder[T, R, op.type]): folder.Out = folder(tuple, z)
    def foldRight[R](z : R)(op : Poly)(using folder: RightFolder[T, R, op.type]): folder.Out = folder(tuple, z)