package shapeless3
package macros

import scala.quoted.*

import shapeless3.labelled.FieldType

object CoproductMacro:
  def deriveCoproductForGeneric[T: Type](using q: Quotes): Expr[Generic[T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    tpe match
      case tpe if tpe.typeSymbol.flags.is(Flags.Sealed) =>
        val subtypes = subtypesOf(q)(tpe)

        if subtypes.isEmpty then
          q.reflect.report.errorAndAbort(s"No concrete subtypes found for sealed trait ${Type.show[T]}")
        else
          val reprType = subtypes.foldRight[TypeRepr](TypeRepr.of[CNil]) { (symbol, t) =>
            val repr = if (symbol.isTerm) symbol.termRef else symbol.typeRef
            TypeRepr.of[:+:].appliedTo(List(repr, t))
          }

          reprType.asType match
            case '[t] =>
              '{ 
                new Generic[T]:
                  type Repr = t
                  def to(t: T): t = ${ constructCoproductExpr[T](q)('t, subtypes) }.asInstanceOf[t]
                  def from(r: t): T = Coproduct.unsafeFromCoproduct(r.asInstanceOf[Coproduct]).asInstanceOf[T]
              }

      case _ =>
        q.reflect.report.errorAndAbort(s"Cannot derive Generic for type ${Type.show[T]}")

  def deriveCoproductForLabelledGeneric[T: Type](using q: Quotes): Expr[LabelledGeneric.Aux[T, ?]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    tpe match
      case tpe if tpe.typeSymbol.flags.is(Flags.Sealed) =>
        val subtypes = subtypesOf(q)(tpe)

        if subtypes.isEmpty then
          q.reflect.report.errorAndAbort(s"No concrete subtypes found for sealed trait ${Type.show[T]}")
        else
          val reprType = subtypes.foldRight[TypeRepr](TypeRepr.of[CNil]) { (symbol, t) =>
            val repr = if (symbol.isTerm) symbol.termRef else symbol.typeRef
            val name = ConstantType(StringConstant(repr.name))
            TypeRepr.of[:+:].appliedTo(List(TypeRepr.of[FieldType].appliedTo(List(name, repr)), t))
          }

          reprType.asType match
            case '[t] =>
              '{ new LabelledGeneric[T] {
                  type Repr = t
                  def to(t: T): t = ${constructCoproductExpr[T](q)('t, subtypes)}.asInstanceOf[t]
                  def from(r: t): T = Coproduct.unsafeFromCoproduct(r.asInstanceOf[Coproduct]).asInstanceOf[T]
                 }.asInstanceOf[LabelledGeneric.Aux[T, t]]
              }

      case _ =>
        q.reflect.report.errorAndAbort(s"Cannot derive LabelledGeneric for type ${Type.show[T]}")

  private def subtypesOf(q: Quotes)(typeRepr: q.reflect.TypeRepr): List[q.reflect.Symbol] =
    import q.reflect.*

    typeRepr.typeSymbol.children.flatMap { symbol =>
      val flags = symbol.flags

      if flags.is(Flags.Case) then
        List(symbol)
      else if (flags.is(Flags.Trait) || flags.is(Flags.Abstract)) && flags.is(Flags.Sealed) then
        subtypesOf(q)(typeRepr.memberType(symbol))
      else
        Nil
    }

  private def constructCoproductExpr[T: Type](q: Quotes)(valueExpr: Expr[T], subtypes: List[q.reflect.Symbol])(using Quotes): Expr[Coproduct] =
    import quotes.reflect.*

    val uncheckedAnnotation = New(TypeIdent(Symbol.requiredClass("scala.unchecked")))

    val cases = subtypes.zipWithIndex.map { case (symbol, int) =>
      val tpe = symbol.typeRef
      val pattern = Typed(Wildcard(), Annotated(TypeTree.of(using tpe.asType), uncheckedAnnotation))
      val body = Expr(int)
      CaseDef(pattern, None, body.asTerm)
    }

    val patternMatch = Match(valueExpr.asTerm, cases).asExprOf[Int]

    '{ Coproduct.unsafeToCoproduct($patternMatch, $valueExpr) }