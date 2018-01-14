package io.scalaland.potok

import org.scalatest.{MustMatchers, WordSpec}

class MigrationTasselSpec
  extends WordSpec with MustMatchers with TodoFixtures {

  "MigrationTasselSpec" when {


    "version is evolved" should {

      def v3TodoCreated = TodoEvent.v3.TodoCreated(1, "", "", Set())

      "provide latest event version" in {

        TodoEvent.todoEventMigrationTassel[TodoEvent.v3.TodoCreated]
          .latestVersion(v3TodoCreated) mustBe 3

      }
    }
  }
}
