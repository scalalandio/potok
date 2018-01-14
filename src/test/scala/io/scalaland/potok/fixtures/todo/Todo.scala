package io.scalaland.potok.fixtures.todo

import java.time.ZonedDateTime

import io.scalaland.potok.Entity

// entity
case class Todo(id: Entity.Id[Int],
                title: String,
                description: String,
                isDone: Boolean,
                tags: Set[String],
                lastModifiedAt: ZonedDateTime) extends Entity[Int]

