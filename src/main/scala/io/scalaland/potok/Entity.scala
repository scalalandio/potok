package io.scalaland.potok

trait Entity[IdT] {
  def id: Entity.Id[IdT]
}

object Entity {
  case class Id[T](value: T) extends AnyVal
}
