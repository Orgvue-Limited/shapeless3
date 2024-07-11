package shapeless3

import _root_.pureconfig.{ConfigCursor, ConfigReader}
import _root_.pureconfig.error.{CannotConvert, ConfigReaderFailures, ConvertFailure, WrongSizeList}

package object pureconfig:
  private[shapeless3] trait SeqShapedReader[Repr] extends ConfigReader[Repr]

  implicit val hNilReader: SeqShapedReader[EmptyTuple] = (cur: ConfigCursor) =>
    cur.asList.flatMap:
      case Nil => Right(EmptyTuple)
      case cl => cur.failed(WrongSizeList(0, cl.size))

  implicit def hConsReader[H, T <: Tuple](implicit
    hr: => ConfigReader[H],
    tr: => SeqShapedReader[T],
    tl: HKernelAux[T]
  ): SeqShapedReader[H *: T] =
    (cur: ConfigCursor) => cur.asListCursor.flatMap {
      case listCur if listCur.size != tl().length + 1 =>
        cur.failed(WrongSizeList(tl().length + 1, listCur.size))

      case listCur =>
        val hv = hr.from(listCur.atIndexOrUndefined(0))
        val tv = tr.from(listCur.tailOption.get)
        ConfigReader.Result.zipWith(hv, tv)(_ *: _)
    }