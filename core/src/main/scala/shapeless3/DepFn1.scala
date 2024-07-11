package shapeless3

trait DepFn1[T]:
  type Out

  def apply(t: T): Out