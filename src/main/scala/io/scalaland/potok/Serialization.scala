package io.scalaland.potok

import io.circe.{Decoder, Encoder}

trait Serializer[T] {
  def serialize(obj: T): Array[Byte]
  def deserialize(payload: Array[Byte]): Option[T]
}

object Serializer {
  implicit def circeSerializer[T : Encoder : Decoder]: Serializer[T] =
    ???
}
