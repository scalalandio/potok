package io.scalaland.potok

import io.scalaland.potok.fixtures.todo._
import org.scalatest.{MustMatchers, WordSpec}

class MigrationTasselSpec
  extends WordSpec with MustMatchers {

  "MigrationTasselSpec" when {

    "event is already in topmost, evolved version" should {

      val v3TodoCreated = TodoEvent.v3.TodoCreated(1, "", "", Set())
      val mt = TodoEvent.todoEventMigrationTassel[TodoEvent.v3.TodoCreated]

      "provide latest event version" in {
        mt.latestVersion(v3TodoCreated) mustBe 3
      }

      "convert to latest version (identity)" in {
        mt.toLatest(v3TodoCreated) mustBe v3TodoCreated
      }
    }

    "event is in older version" should {

      val v1TodoDeleted = TodoEvent.v1.TodoDeleted(1)
      val mt = TodoEvent.todoEventMigrationTassel[TodoEvent.v1.TodoDeleted]

      "provide latest event version" in {
        mt.latestVersion(v1TodoDeleted) mustBe 2
      }

      "convert to latest version applying migration" in {
        mt.toLatest(v1TodoDeleted) mustBe TodoEvent.v2.TodoDeleted(1, permanent = true)
      }
    }

    "event is in topmost v1" should {

      val v1TodoMarkedDone = TodoEvent.v1.TodoMarkedDone(1)
      val mt = TodoEvent.todoEventMigrationTassel[TodoEvent.v1.TodoMarkedDone]


      "provide latest event version" in {
        mt.latestVersion(v1TodoMarkedDone) mustBe 1
      }

      "convert to latest version (identity)" in {
        mt.toLatest(v1TodoMarkedDone) mustBe v1TodoMarkedDone
      }
    }
  }
}
