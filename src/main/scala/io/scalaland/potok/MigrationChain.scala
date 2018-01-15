package io.scalaland.potok

import shapeless.ops.coproduct.Inject
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}


trait MigrationChain[LatestType] { self =>
  type PreviousTypes <: Coproduct

  def latestVersion: Int

  def toLatest[U](value: U)(implicit inject: Inject[LatestType :+: PreviousTypes, U]): LatestType =
    toLatest(inject(value))

  def toLatest(injected: LatestType :+: PreviousTypes): LatestType

  def to[T](migrate: LatestType => T): MigrationChain.Aux[T, LatestType :+: PreviousTypes] =
    new MigrationChain[T] {
      val latestVersion: Int = self.latestVersion + 1
      type PreviousTypes = LatestType :+: self.PreviousTypes
      def toLatest(injected: T :+: PreviousTypes): T = injected match {
        case Inl(latest) => latest
        case Inr(previous) => migrate(self.toLatest(previous))
      }
    }
}

object MigrationChain {

  type Aux[LatestType, PrevTypes <: Coproduct] =
    MigrationChain[LatestType] { type PreviousTypes = PrevTypes }

  def from[LatestType]: MigrationChain.Aux[LatestType, CNil] = new MigrationChain[LatestType] {
    def latestVersion: Int = 1
    type PreviousTypes = CNil
    def toLatest(injected: LatestType :+: PreviousTypes): LatestType = injected match {
      case Inl(latest) => latest
      case Inr(_) => ???
    }
  }
}
