package io.scalaland.potok

import org.scalatest.{MustMatchers, WordSpec}

class MigrationChainSpec
  extends WordSpec with MustMatchers with TodoFixtures {

  "MigrationSpec" should {

    "return latest version number" in {

      TodoEvent.todoCreatedMigrationChain.latestVersion mustBe 3
    }

    "migrate from latest version" in {

      val latestTodoCreated = TodoEvent.v3.TodoCreated(1, "", "", Set())

      TodoEvent.todoCreatedMigrationChain.toLatest(latestTodoCreated) mustBe latestTodoCreated
    }

    "migrate from older versions" in {
      val v1TodoCreated = TodoEvent.v1.TodoCreated(1, "")
      val v2TodoCreated = TodoEvent.v2.TodoCreated(1, "", "")
      val latestTodoCreated = TodoEvent.v3.TodoCreated(1, "", "", Set())

      TodoEvent.todoCreatedMigrationChain.toLatest(v1TodoCreated) mustBe latestTodoCreated
      TodoEvent.todoCreatedMigrationChain.toLatest(v2TodoCreated) mustBe latestTodoCreated
    }
  }
}