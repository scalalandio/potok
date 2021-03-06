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

    "given non-evolved event" should {

      "serialize and deserialize" in {

        val v1TodoTitleUpdated = TodoEvent.v1.TodoTitleUpdated(1, "test")

        val rawEventEnvelope = st.serialize(v1TodoTitleUpdated)

        inside(rawEventEnvelope.header) { case header =>
          header.`type` mustBe "io.scalaland.potok.fixtures.todo.TodoEvent$v1$TodoTitleUpdated"
          header.version mustBe 1
        }

        rawEventEnvelope.payload mustBe
          """{"id":1,"title":"test"}""".getBytes("UTF-8")

        st.deserialize(rawEventEnvelope) mustBe Some {
          EventEnvelope(rawEventEnvelope.header, v1TodoTitleUpdated)
        }
      }
    }

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


    "given non-last version of evolved event" should {

      "deserialize and apply evolution from first version" in {

        val v1TodoCreated = TodoEvent.v1.TodoCreated(1, "")

        val v1Serializer = implicitly[Serializer[TodoEvent.v1.TodoCreated]]

        val v1EventEnvelope = RawEventEnvelope(
          header = EventHeader(v1Serializer.eventName), // v1
          payload = v1Serializer.serialize(v1TodoCreated)
        )

        st.deserialize(v1EventEnvelope) mustBe Some {
          EventEnvelope(v1EventEnvelope.header, v1TodoCreated.toV2.toV3)
        }
      }

      "deserialize and apply evolution from middle version" in {

        val v2TodoCreated = TodoEvent.v2.TodoCreated(1, "", "")

        val v2Serializer = implicitly[Serializer[TodoEvent.v2.TodoCreated]]

        val v2EventEnvelope = RawEventEnvelope(
          header = EventHeader(v2Serializer.eventName, version = 2), // v2
          payload = v2Serializer.serialize(v2TodoCreated)
        )

        st.deserialize(v2EventEnvelope) mustBe Some {
          EventEnvelope(v2EventEnvelope.header, v2TodoCreated.toV3)
        }
      }
    }
  }
}
