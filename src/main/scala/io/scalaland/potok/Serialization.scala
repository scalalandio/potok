package io.scalaland.potok

import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import io.circe.parser.parse
import shapeless.ops.coproduct.Inject
import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr}

import scala.reflect.ClassTag

trait Serializer[T] {
  def eventName: String

  def serialize(event: T): Array[Byte]

  def deserialize(payload: Array[Byte]): Option[T]
}

object Serializer {
  implicit def circeSerializer[T: Encoder : Decoder : ClassTag]: Serializer[T] = new Serializer[T] {
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

object SerializationTassel extends LowPrioritySerializationTassel {

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

  implicit def hasMCCase[H, T <: Coproduct, PrevTypes <: Coproduct](implicit  mc: MigrationChain.Aux[H, PrevTypes],
                                                                    dc: DeserializationChain[H, PrevTypes],
                                                                    s: Serializer[H],
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
        if (eventEnvelope.header.`type` == s.eventName) {
          s.deserialize(eventEnvelope.payload).map { decodedPayload =>
            EventEnvelope[H :+: T](
              header = eventEnvelope.header,
              payload = Inl(decodedPayload)
            )
          }
        } else {
          dc.tryDeserialize(eventEnvelope).map { decodedPayload =>
            EventEnvelope[H :+: T](
              header = eventEnvelope.header,
              payload = Inl(decodedPayload)
            )
          }.orElse {
            stTail.deserialize(eventEnvelope)
              .map(ee => ee.copy(payload = Inr(ee.payload)))
          }
        }
      }
    }
}

trait LowPrioritySerializationTassel {
  implicit def hasNoMCCase[H, T <: Coproduct](implicit s: Serializer[H],
                                              stTail: SerializationTassel[T]): SerializationTassel[H :+: T] =
    new SerializationTassel[H :+: T] {
      def serialize(event: H :+: T): RawEventEnvelope = event match {
        case Inl(head) =>
          RawEventEnvelope(
            header = EventHeader(`type` = s.eventName), // version = 1
            payload = s.serialize(head)
          )
        case Inr(tail) =>
          stTail.serialize(tail)
      }

      def deserialize(eventEnvelope: RawEventEnvelope): Option[EventEnvelope[H :+: T]] = {
        if (eventEnvelope.header.`type` == s.eventName) {
          s.deserialize(eventEnvelope.payload).map { decodedPayload =>
            EventEnvelope(
              header = eventEnvelope.header,
              payload = Inl(decodedPayload)
            )
          }
        } else {
          stTail.deserialize(eventEnvelope).map(ee => ee.copy(payload = Inr(ee.payload)))
        }
      }
    }
}

trait DeserializationChain[Target, PrevTypes <: Coproduct] {
  def tryDeserialize(rawEventEnvelope: RawEventEnvelope): Option[Target]
}

object DeserializationChain {

  implicit def nilCase[Target](implicit mc: MigrationChain.Aux[Target, CNil]): DeserializationChain[Target, CNil] =
    new DeserializationChain[Target, CNil] {
      def tryDeserialize(rawEventEnvelope: RawEventEnvelope): Option[Target] = None
    }

  implicit def prevCons[Target, PrevType, PrevTail <: Coproduct](implicit mc: MigrationChain.Aux[Target, PrevType :+: PrevTail],
                                                                 serializer: Serializer[PrevType],
                                                                 inj: Inject[PrevType :+: PrevTail, PrevType],
                                                                 dc: DeserializationChain[Target, PrevTail]): DeserializationChain[Target, PrevType :+: PrevTail] =
    new DeserializationChain[Target, PrevType :+: PrevTail] {
      def tryDeserialize(rawEventEnvelope: RawEventEnvelope): Option[Target] =
        if(rawEventEnvelope.header.`type` == serializer.eventName) {
          serializer.deserialize(rawEventEnvelope.payload)
            .map(prev => mc.toLatest(prev))
        } else {
          dc.tryDeserialize(rawEventEnvelope)
        }
    }
}
