package io.scalaland.potok

import io.circe.generic.auto._
import io.scalaland.potok.fixtures.todo._
import org.scalatest.{Inside, MustMatchers, WordSpec}

class SerializationSpec
  extends WordSpec with MustMatchers with Inside {

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

  "SerializationTassel" when {

    val st = implicitly[SerializationTassel[TodoEvent]]

    "given last version of evolved event" should {

      "serialize and deserialize" in {

        val v3TodoCreated = TodoEvent.v3.TodoCreated(1, "", "", Set())

        val rawEventEnvelope = st.serialize(v3TodoCreated)

        inside(rawEventEnvelope.header) { case header =>
          header.`type` mustBe "io.scalaland.potok.fixtures.todo.TodoEvent$v3$TodoCreated"
          header.version mustBe 3
        }

        rawEventEnvelope.payload mustBe
          """{"id":1,"title":"","description":"","tags":[]}""".getBytes("UTF-8")

        st.deserialize(rawEventEnvelope) mustBe Some {
          EventEnvelope(rawEventEnvelope.header, v3TodoCreated)
        }
      }
    }
  }
}
