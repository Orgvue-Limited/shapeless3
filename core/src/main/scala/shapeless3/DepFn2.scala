package shapeless3

trait DepFn2[T, U]:
  type Out

  def apply(t: T, u: U): Out