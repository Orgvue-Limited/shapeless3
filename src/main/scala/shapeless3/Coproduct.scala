package shapeless3

sealed trait Coproduct extends Product with Serializable

sealed trait :+:[+H, +T <: Coproduct] extends Coproduct:
  def eliminate[A](l: H => A, r: T => A): A

final case class Inl[+H, +T <: Coproduct](head: H) extends :+:[H, T]:
  override def eliminate[A](l: H => A, r: T => A) = l(head)

final case class Inr[+H, +T <: Coproduct](tail: T) extends :+:[H, T]:
  override def eliminate[A](l: H => A, r: T => A) = r(tail)

sealed trait CNil extends Coproduct:
  def impossible: Nothing

object Coproduct:
  def unsafeToCoproduct(length: Int, value: Any): Coproduct =
    (0 until length).foldLeft[Coproduct](Inl(value))((c, _) => Inr(c))

  @scala.annotation.tailrec
  def unsafeFromCoproduct(c: Coproduct): Any = c match
    case Inl(h)  => h
    case Inr(c)  => unsafeFromCoproduct(c)
    case _: CNil => sys.error("impossible")

  type ToCoproduct[T <: Tuple] <: Coproduct = T match
    case EmptyTuple => CNil
    case h *: t     => h :+: ToCoproduct[t]

  type ToTuple[C <: Coproduct] <: Tuple = C match
    case CNil    => EmptyTuple
    case h :+: t => h *: ToTuple[t]