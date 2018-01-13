package io.scalaland.potok

import java.util.UUID

case class EventHeader(`type`: String,
                       id: UUID = UUID.randomUUID(),
                       timestamp: Long = System.currentTimeMillis(),
                       tags: Set[String] = Set.empty,
                       version: Int = 1,
                       offset: Long = 0)

case class RawEventEnvelope(header: EventHeader, payload: Array[Byte])


case class EventEnvelope[T](header: EventHeader,
                            payload: T)
