package shapeless3.labelled

import scala.deriving.*
import scala.compiletime.*

import shapeless3.DepFn1

trait LabelledType[T] extends DepFn1[T] with Serializable:
  type Out <: Tuple

object LabelledType:
  type Aux[T, Out0] = LabelledType[T] { type Out = Out0 }

  def apply[T](implicit lab: LabelledType[T]): Aux[T, lab.Out] = lab

  type FieldTypes[Labels <: Tuple, Types <: Tuple] <: Tuple =
    (Labels, Types) match
      case (labelHead *: labelTail, typeHead *: typeTail) =>
        FieldType[labelHead, typeHead] *: FieldTypes[labelTail, typeTail]
      case _                                              =>
        EmptyTuple

  def fieldTypes[Labels <: Tuple, Types <: Tuple](labels: Labels, types: Types): FieldTypes[Labels, Types] =
    (labels, types) match
      case (labelHead *: labelTail, typeHead *: typeTail) =>
        (field(typeHead) *: fieldTypes(labelTail, typeTail)).asInstanceOf[FieldTypes[Labels, Types]]
      case _                                              =>
        EmptyTuple.asInstanceOf[FieldTypes[Labels, Types]]

  inline given [T <: Product](using m: Mirror.ProductOf[T]): LabelledType.Aux[T, FieldTypes[m.MirroredElemLabels, m.MirroredElemTypes]] =
    new LabelledType[T]:
      override type Out = FieldTypes[m.MirroredElemLabels, m.MirroredElemTypes]
      override def apply(t: T): Out =
        fieldTypes[m.MirroredElemLabels, m.MirroredElemTypes](
          constValueTuple[m.MirroredElemLabels],
          Tuple.fromProductTyped(t)
        )