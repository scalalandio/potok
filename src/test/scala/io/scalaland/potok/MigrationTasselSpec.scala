package io.scalaland.potok

import io.scalaland.potok.fixtures.todo._
import org.scalatest.{MustMatchers, WordSpec}

class MigrationTasselSpec
  extends WordSpec with MustMatchers {

  "MigrationTasselSpec" when {

    "version is topmost" should {

      def v3TodoCreated = TodoEvent.v3.TodoCreated(1, "", "", Set())

      "provide latest event version" in {

        TodoEvent.todoEventMigrationTassel[TodoEvent.v3.TodoCreated]
          .latestVersion(v3TodoCreated) mustBe 3

      }
    }

    "version is older" should {

      def v1TodoDeleted = TodoEvent.v1.TodoDeleted(1)

      "provide latest event version" in {
        TodoEvent.todoEventMigrationTassel[TodoEvent.v1.TodoDeleted]
          .latestVersion(v1TodoDeleted) mustBe 2
       }
    }

    "version is not touched" should {

      def v1TodoMarkedDone = TodoEvent.v1.TodoMarkedDone(1)

      "provide latest event version" in {
        TodoEvent.todoEventMigrationTassel[TodoEvent.v1.TodoMarkedDone]
          .latestVersion(v1TodoMarkedDone) mustBe 1
      }
    }


  }
}
