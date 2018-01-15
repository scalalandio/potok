package io.scalaland.potok

import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import io.circe.parser.parse
import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr}

import scala.reflect.ClassTag

trait Serializer[T] {
  def eventName: String
  def serialize(event: T): Array[Byte]
  def deserialize(payload: Array[Byte]): Option[T]
}

object Serializer {
  implicit def circeSerializer[T : Encoder : Decoder : ClassTag]: Serializer[T] = new Serializer[T] {
    val eventName = implicitly[ClassTag[T]].runtimeClass.getName
    def serialize(event: T): Array[Byte] =
      event.asJson.noSpaces.getBytes("UTF-8")
    def deserialize(payload: Array[Byte]): Option[T] =
      parse(new String(payload, "UTF-8")).toOption.flatMap(_.as[T].toOption)
  }
}

trait SerializationTassel[Event] {
  def serialize(event: Event): RawEventEnvelope
  def deserialize(eventEnvelope: RawEventEnvelope): Option[EventEnvelope[Event]]
}

object SerializationTassel {

  implicit def gen[Event, Gen <: Coproduct](implicit gen: Generic.Aux[Event, Gen],
                                            st: SerializationTassel[Gen]): SerializationTassel[Event] =
    new SerializationTassel[Event] {

      def serialize(event: Event): RawEventEnvelope =
        st.serialize(gen.to(event))

      def deserialize(eventEnvelope: RawEventEnvelope): Option[EventEnvelope[Event]] =
        st.deserialize(eventEnvelope).map(ee => ee.copy(payload = gen.from(ee.payload)))
    }

  implicit val cnilCase: SerializationTassel[CNil] = new SerializationTassel[CNil] {
    def serialize(event: CNil): RawEventEnvelope = ???
    def deserialize(eventEnvelope: RawEventEnvelope): Option[EventEnvelope[CNil]] =
      None
  }

  implicit def cconsCase[H, T <: Coproduct](implicit s: Serializer[H],
                                            mc: MigrationChain[H],
                                            stTail: SerializationTassel[T]): SerializationTassel[H :+: T] =
    new SerializationTassel[H :+: T] {
      def serialize(event: H :+: T): RawEventEnvelope = event match {
        case Inl(head) =>
          RawEventEnvelope(
            header = EventHeader(
              `type` = s.eventName,
              version = mc.latestVersion
            ),
            payload = s.serialize(head)
          )
        case Inr(tail) =>
          stTail.serialize(tail)
      }

      def deserialize(eventEnvelope: RawEventEnvelope): Option[EventEnvelope[H :+: T]] = {
        if(eventEnvelope.header.`type` == s.eventName) {
          s.deserialize(eventEnvelope.payload).map { decodedPayload =>
            EventEnvelope(
              header = eventEnvelope.header,
              payload = Inl(decodedPayload)
            )
          }
        } else if(false) {
          // TODO: deserialize & migrate case
          // TODO: check migration chain for coproduct of previous types
          // TODO: for each derive serializer
          // TODO: check if any deserializer is able to deserialize that to some type U
          // TODO: if yes, use migration chain to migrate it to H
          // TODO: encode H as Inl
          // TODO: possibly separate type class for H needs to be written (SerializationChain?)
          None
        } else {
          stTail.deserialize(eventEnvelope).map(ee => ee.copy(payload = Inr(ee.payload)))
        }
      }
    }
}