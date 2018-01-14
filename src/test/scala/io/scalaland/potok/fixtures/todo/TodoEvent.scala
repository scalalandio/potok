package io.scalaland.potok.fixtures.todo

import io.scalaland.potok.{MigrationChain, MigrationTassel}

sealed trait TodoEvent

object TodoEvent {

  object v1 {

    case class TodoCreated(id: Int, title: String) {
      def toV2: v2.TodoCreated = v2.TodoCreated(id, title, "")
    }

    case class TodoTitleUpdated(id: Int, title: String) extends TodoEvent

    case class TodoMarkedDone(id: Int) extends TodoEvent

    case class TodoMarkedUndone(id: Int) extends TodoEvent

    case class TodoDescriptionUpdated(id: Int, description: String) extends TodoEvent

    case class TodoDeleted(id: Int) {
      def toV2 = v2.TodoDeleted(id, permanent = true)
    }

  }

  object v2 {

    case class TodoCreated(id: Int, title: String, description: String) {
      def toV3 = v3.TodoCreated(id, title, description, Set())
    }

    case class TodoDeleted(id: Int, permanent: Boolean) extends TodoEvent

  }

  object v3 {

    case class TodoCreated(id: Int, title: String, description: String, tags: Set[String]) extends TodoEvent

    case class TodoUpdateTags(id: Int, tags: Set[String]) extends TodoEvent

  }

  implicit val todoCreatedMigrationChain =
    MigrationChain
      .from[v1.TodoCreated]
      .to[v2.TodoCreated](_.toV2)
      .to[v3.TodoCreated](_.toV3)

  implicit val todoDeletedMigrationChain =
    MigrationChain
      .from[v1.TodoDeleted]
      .to[v2.TodoDeleted](_.toV2)

  implicit def todoEventMigrationTassel[U](implicit mt: MigrationTassel[TodoEvent, U]) = mt
}
