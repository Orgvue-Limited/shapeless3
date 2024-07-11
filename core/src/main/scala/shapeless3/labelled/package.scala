package shapeless3

package object labelled:
  opaque type FieldType[K, +V] <: V = V

  type ->>[K, +V] = FieldType[K, V]

  def field[K]: FieldBuilder[K] = new FieldBuilder(true)

  class FieldBuilder[K](private val dummy: Boolean) extends AnyVal:
    def apply[V](v: V): FieldType[K, V] = v.asInstanceOf[FieldType[K, V]]