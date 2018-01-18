package io.scalaland.potok

import io.circe.generic.auto._
import io.scalaland.potok.fixtures.todo._
import org.scalatest.{MustMatchers, WordSpec}

class SerializationSpec
  extends WordSpec with MustMatchers {

  "Serialization" should {

    "work 2-ways when derived for case class with circe" in {

      val v1TodoCreated = TodoEvent.v1.TodoCreated(1, "")
      val v1TodoCreatedSerializer = implicitly[Serializer[TodoEvent.v1.TodoCreated]]

      v1TodoCreatedSerializer.eventName mustBe "io.scalaland.potok.fixtures.todo.TodoEvent$v1$TodoCreated"

      val serializedBytes = v1TodoCreatedSerializer.serialize(v1TodoCreated)
      serializedBytes mustBe """{"id":1,"title":""}""".getBytes("UTF-8")

      val deserialized = v1TodoCreatedSerializer.deserialize(serializedBytes)
      deserialized mustBe Some(v1TodoCreated)
    }
  }
}
