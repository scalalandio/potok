package io.scalaland.potok

import shapeless.ops.coproduct.{Inject, Length}
import shapeless.ops.nat.ToInt
import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr, Lazy, Nat}


trait MigrationTassel[Event, U] {

  def latestVersion(event: U): Int
  def toLatest(value: U): Event
}

object MigrationTassel {


  implicit def gen[Event, U, Gen <: Coproduct](implicit gen: Generic.Aux[Event, Gen],
                                               mt: Lazy[MigrationTassel[Gen, U]]): MigrationTassel[Event, U] =
    new MigrationTassel[Event, U] {
      def latestVersion(event: U): Int =
        mt.value.latestVersion(event)
      def toLatest(value: U): Event =
        gen.from(mt.value.toLatest(value))
    }

  implicit def cnilCase[U]: MigrationTassel[CNil, U] = new MigrationTassel[CNil, U] {
    def latestVersion(event: U): Int = 1
    def toLatest(value: U): CNil = ???
  }

  implicit def cconsCase[T, Ts <: Coproduct, U, PTs <: Coproduct, N <: Nat]
    (implicit mc: Lazy[MigrationChain.Aux[T, PTs]],
     injU: Inject[T :+: PTs, U],
     injT: Inject[T :+: Ts, T],
     length: Length.Aux[PTs, N],
     toInt: ToInt[N]): MigrationTassel[T :+: Ts, U] =
    new MigrationTassel[T :+: Ts, U] {
      def latestVersion(event: U): Int =
        mc.value.latestVersion

      def toLatest(value: U): T :+: Ts =
        injT(mc.value.toLatest(value))
    }

  implicit def ctailCase[T, Ts <: Coproduct, U]
    (implicit mt: Lazy[MigrationTassel[Ts, U]]): MigrationTassel[T :+: Ts, U] =
    new MigrationTassel[T :+: Ts, U] {
      def latestVersion(event: U): Int =
        mt.value.latestVersion(event)

      def toLatest(value: U): T :+: Ts =
        Inr(mt.value.toLatest(value))
    }

}