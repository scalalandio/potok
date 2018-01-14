package io.scalaland.potok

import shapeless.ops.coproduct.{Inject, Length}
import shapeless.ops.nat.ToInt
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, Nat, Succ, _0}


trait MigrationChain[LatestType] { self =>
  type PreviousTypes <: Coproduct

  def latestVersion[N <: Nat](implicit length: Length.Aux[PreviousTypes, N],
                              toInt: ToInt[N]): Int =
    1 + toInt()

  def toLatest[U](value: U)(implicit inject: Inject[LatestType :+: PreviousTypes, U]): LatestType =
    toLatest(inject(value))

  def toLatest(injected: LatestType :+: PreviousTypes): LatestType

  def to[T](migrate: LatestType => T): MigrationChain.Aux[T, LatestType :+: PreviousTypes] =
    new MigrationChain[T] {
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
    type PreviousTypes = CNil
    def toLatest(injected: LatestType :+: PreviousTypes): LatestType = injected match {
      case Inl(latest) => latest
      case Inr(_) => ???
    }
  }
}
