package io.scalaland.potok

import akka.NotUsed
import akka.stream.scaladsl.Source

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future

trait EventStorage {
  def storeEvents(events: RawEventEnvelope*): Future[Long]
  def storeEvent(event: RawEventEnvelope): Future[Long] = storeEvents(event)
  def readAllEvents(afterOffset: Option[Long] = None): Source[RawEventEnvelope, NotUsed]
  def readAllEventsByTag(tag: String, afterOffset: Option[Long] = None): Source[RawEventEnvelope, NotUsed]
  def readEvents(afterOffset: Option[Long] = None): Source[RawEventEnvelope, NotUsed]
  def readEventsByTag(tag: String, afterOffset: Option[Long] = None): Source[RawEventEnvelope, NotUsed]
}

object EventStorage {
  def inMemory: EventStorage = new EventStorage {
    val journal: ArrayBuffer[RawEventEnvelope] = ArrayBuffer.empty

    def storeEvents(events: RawEventEnvelope*): Future[Long] =
      Future.successful {
        val nextOffset = journal.lastOption.map(_.header.offset).getOrElse(0L) + 1L
        val eventsWithOffset = events.zipWithIndex.map { case (e, i) =>
          e.copy(header = e.header.copy(offset = nextOffset + i))
        }
        journal.append(eventsWithOffset: _*)
        nextOffset + events.length
      }

    def readAllEvents(afterOffset: Option[Long] = None): Source[RawEventEnvelope, NotUsed] = {
      afterOffset match {
        case Some(offset) =>
          Source.fromIterator(() => journal.iterator).filter(_.header.offset > offset)
        case None =>
          Source.fromIterator(() => journal.iterator)
      }
    }

    def readAllEventsByTag(tag: String, afterOffset: Option[Long] = None): Source[RawEventEnvelope, NotUsed] =
      readAllEvents(afterOffset).filter(_.header.tags.contains(tag))

    def readEvents(afterOffset: Option[Long] = None): Source[RawEventEnvelope, NotUsed] =
      readAllEvents(afterOffset) // TODO: add real support for live events

    def readEventsByTag(tag: String, afterOffset: Option[Long] = None): Source[RawEventEnvelope, NotUsed] =
      readEvents(afterOffset).filter(_.header.tags.contains(tag))
  }
}