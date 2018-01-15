package io.scalaland.potok

import shapeless.ops.coproduct.Inject
import shapeless.{:+:, CNil, Coproduct, Generic, Inr, Lazy, Nat}


trait MigrationTassel[Event, U] {

  def latestVersion: Int
  def toLatest(event: U): Event
}

object MigrationTassel extends LowPriorityMigrationTassel0 {

  def apply[Event, U](implicit mt: MigrationTassel[Event, U]): MigrationTassel[Event, U] = mt

  implicit def derive[Event, U, Gen <: Coproduct](implicit gen: Generic.Aux[Event, Gen],
                                                  mt: Lazy[MigrationTassel[Gen, U]]): MigrationTassel[Event, U] =
    new MigrationTassel[Event, U] {
      def latestVersion: Int =
        mt.value.latestVersion
      def toLatest(value: U): Event =
        gen.from(mt.value.toLatest(value))
    }

  implicit def cnilCase[U]: MigrationTassel[CNil, U] = new MigrationTassel[CNil, U] {
    val latestVersion: Int = 1
    def toLatest(value: U): CNil = ???
  }

  implicit def mcCase[T, Ts <: Coproduct, U, PTs <: Coproduct, N <: Nat]
    (implicit mc: Lazy[MigrationChain.Aux[T, PTs]],
     injU: Inject[T :+: PTs, U],
     injT: Inject[T :+: Ts, T]): MigrationTassel[T :+: Ts, U] =
    new MigrationTassel[T :+: Ts, U] {
      def latestVersion: Int =
        mc.value.latestVersion

      def toLatest(value: U): T :+: Ts =
        injT(mc.value.toLatest(value))
    }
}

trait LowPriorityMigrationTassel0 extends LowPriorityMigrationTassel1 {

  implicit def noMCCase[T, Ts <: Coproduct, U, PTs <: Coproduct, N <: Nat]
  (implicit ev: U =:= T,
   injT: Inject[T :+: Ts, T]): MigrationTassel[T :+: Ts, U] =
    new MigrationTassel[T :+: Ts, U] {
      def latestVersion: Int = 1

      def toLatest(value: U): T :+: Ts = injT(ev(value))
    }
}

trait LowPriorityMigrationTassel1 {
  implicit def ctailCase[T, Ts <: Coproduct, U]
  (implicit mt: Lazy[MigrationTassel[Ts, U]]): MigrationTassel[T :+: Ts, U] =
    new MigrationTassel[T :+: Ts, U] {
      def latestVersion: Int =
        mt.value.latestVersion

      def toLatest(value: U): T :+: Ts =
        Inr(mt.value.toLatest(value))
    }
}