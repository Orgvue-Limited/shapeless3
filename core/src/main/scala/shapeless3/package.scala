package object shapeless3:
  type Id[+T] = T

  type Const[C] = {
    type λ[T] = C
  }