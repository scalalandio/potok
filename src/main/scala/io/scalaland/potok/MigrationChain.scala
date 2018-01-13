package io.scalaland.potok

import shapeless.ops.coproduct.{Inject, Length}
import shapeless.ops.nat.ToInt
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, Nat, Succ, _0}


trait MigrationChain[LatestType, PreviousTypes <: Coproduct] { self =>

  def latestVersion[N <: Nat](implicit length: Length.Aux[PreviousTypes, N],
                              toInt: ToInt[N]): Int =
    1 + toInt()

  def toLatest[U](value: U)(implicit inject: Inject[LatestType :+: PreviousTypes, U]): LatestType =
    toLatest(inject(value))

  def toLatest(injected: LatestType :+: PreviousTypes): LatestType

  def to[T](migrate: LatestType => T): MigrationChain[T, LatestType :+: PreviousTypes] = {
    case Inl(latest) => latest
    case Inr(previous) => migrate(self.toLatest(previous))
  }
}

object MigrationChain {

  def from[LatestType]: MigrationChain[LatestType, CNil] = {
    case Inl(latest) => latest
    case Inr(_) => ???
  }
}
