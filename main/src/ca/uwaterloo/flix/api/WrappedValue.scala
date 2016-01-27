package ca.uwaterloo.flix.api

import java.util

import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.runtime.Value

import scala.collection.immutable

protected final class WrappedValue(val ref: AnyRef) extends IValue {

  def isUnit: Boolean = ref match {
    case Value.Unit => true
    case _ => false
  }

  // TODO: Maybe this needs to be part of the value???
  @deprecated
  def getType: IType = ref match {
    case Value.Unit => new WrappedType(Type.Unit)
    case Value.True => new WrappedType(Type.Bool)
    case Value.False => new WrappedType(Type.Bool)
    case v: Value.Int => new WrappedType(Type.Int32)
    case v: Value.Str => new WrappedType(Type.Str)
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getBool: Boolean = ref match {
    case Value.True => true
    case Value.False => false
    case o: java.lang.Boolean => o.booleanValue()
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def isTrue: Boolean = getBool

  def isFalse: Boolean = !getBool

  def getChar: Char = ref match {
    case o: java.lang.Character => o.charValue()
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getInt8: Byte = ref match {
    case o: java.lang.Byte => o.byteValue()
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getInt16: Short = ref match {
    case o: java.lang.Short => o.shortValue()
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getInt32: Int = ref match {
    case o: java.lang.Integer => o.intValue()
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getInt64: Long = ref match {
    case o: java.lang.Long => o.longValue()
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getStr: String = ref match {
    case v: Value.Str => v.s
    case o: java.lang.String => o
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getTuple: Array[IValue] = ref match {
    case v: Value.Tuple => v.elms.map(e => new WrappedValue(e))
    case o: Array[AnyRef] => o.map(e => new WrappedValue(e))
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getEnumName: String = ref match {
    case v: Value.Tag => v.enum.parts.mkString("::")
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getTagName: String = ref match {
    case v: Value.Tag => v.tag
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getTagValue: IValue = ref match {
    case v: Value.Tag => new WrappedValue(v.value)
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getNativeObj: AnyRef = ref match {
    case v: Value.Native => v.value
    case o => o
  }

  def getJavaOpt: java.util.Optional[IValue] = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.") // TODO

  def getScalaOpt: scala.Option[IValue] = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.") // TODO

  def getJavaList: java.util.Set[IValue] = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.") // TODO

  def getScalaList: immutable.List[IValue] = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.") // TODO

  def getJavaSet: java.util.Set[IValue] = ref match {
    case v: Value.Set => {
      val r = new util.HashSet[IValue]()
      for (e <- v.elms) {
        r.add(new WrappedValue(e))
      }
      r
    }
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getScalaSet: immutable.Set[IValue] = ref match {
    case v: Value.Set => v.elms.map(e => new WrappedValue(e))
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def getJavaMap: java.util.Map[IValue, IValue] = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.") // TODO

  def getScalaMap: immutable.Map[IValue, IValue] = throw new UnsupportedOperationException("Not Yet Implemented. Sorry.") // TODO

  override def equals(other: Any): Boolean = other match {
    case that: WrappedValue => ref == that.ref
    case _ => false
  }

  override def hashCode(): Int = ref.hashCode()

}