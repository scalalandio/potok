package io.scalaland.potok

import scala.concurrent.Future

trait OffsetStorage {
  def commitOffset(name: String, offset: Long): Future[Unit]
  def fetchLastCommittedOffset(name: String): Future[Option[Long]]
}

object OffsetStorage {
  def inMemory: OffsetStorage = new OffsetStorage {
    var offsets: Map[String, Long] = Map.empty
    def fetchLastCommittedOffset(name: String): Future[Option[Long]] =
      Future.successful(offsets.get(name))
    def commitOffset(name: String, offset: Long): Future[Unit] =
      Future.successful(offsets += name -> offset)
  }
}
