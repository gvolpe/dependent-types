package com.github.gvolpe.types.typelevel

import scala.language.higherKinds

object NatTypeLevel extends App {

  sealed trait Nat {
    type This >: this.type <: Nat
    type ++ = Succ[This]
    type + [_ <: Nat] <: Nat
    type * [_ <: Nat] <: Nat
  }

  object Zero extends Nat {
    type This = Zero
    type + [X <: Nat] = X
    type * [X <: Nat] = Zero
  }

  class Succ[N <: Nat] extends Nat {
    type This = Succ[N]
    type + [X <: Nat] = Succ[N# + [X]]
    type * [X <: Nat] = (N# + [X])# + [X]
  }

  type Zero   = Zero.type
  type One    = Zero# ++
  type Two    = One# ++
  type Three  = Two# ++
  type Four   = Three# ++
  type Five   = Four# ++

  implicitly[Two# + [Three] =:= Five]
  implicitly[One# + [Two] =:= Three]
  //  implicitly[Two# + [Three] =:= Four] // Does not compile

  sealed trait Factorial[N <: Nat] { type Res <: Nat }

  implicit object factorial0 extends Factorial[Zero] { type Res = One }

  implicit def factorial[N <: Nat, X <: Nat](implicit fact: Factorial[N] { type Res = X}) =
    new Factorial[Succ[N]] { type Res = X# + [Succ[N]] }

  implicitly[Factorial[Zero] { type Res = One }]
  //  implicitly[Factorial[Three] { type Res = Five }] // Does not compile!

  sealed trait Fibonacci[N <: Nat] { type Res <: Nat }

  implicit object fibonacci0 extends Fibonacci[Zero] { type Res = Zero }

  implicit object fibonacci1 extends Fibonacci[One] { type Res = One }

  implicit def fibonacci[N <: Nat, X <: Nat, Y <: Nat](implicit fib1: Fibonacci[N] { type Res = X}, fib2: Fibonacci[Succ[N]] { type Res = Y }) =
    new Fibonacci[Succ[Succ[N]]] { type Res = X# + [Y] }

  implicitly[Fibonacci[Two] { type Res = One }]
  implicitly[Fibonacci[Three] { type Res = Two }]
  implicitly[Fibonacci[Four] { type Res = Three }]
  implicitly[Fibonacci[Five] { type Res = Five }]
//  implicitly[Fibonacci[Five] { type Res = Two }] // Does not compile!

}