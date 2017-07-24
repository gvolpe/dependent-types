package com.github.gvolpe.types

/**
  * Dependent Pair Type, also called Σ-Type (Sigma Type)
  *
  * In the Idris language this is supported by default:
  *
  * depType : Int -> Type
  * depType 0 = Int
  * depType 1 = String
  * depType _ = Bool
  *
  * data DepPair : (a : Type) -> (P : a -> Type) -> Type where
  *     MakeDepPair : {P : a -> Type} -> (x : a) -> P x -> DepPair a P
  *
  * x : DepPair Int (\n => depType n)
  * x = MakeDepPair 0 10
  *
  * y : DepPair Int (\n => depType n)
  * y = MakeDepPair 1 "abc"
  *
  * z : DepPair Int (\n => depType n)
  * z = MakeDepPair 2 True
  * */
object DependentPairType {

  sealed trait Nat {
    type This >: this.type <: Nat
    type ++ = Succ[This]
  }

  object Zero extends Nat {
    type This = Zero
  }
  type Zero = Zero.type

  class Succ[N <: Nat] extends Nat {
    type This = Succ[N]
  }

  type _0 = Zero
  type _1 = _0# ++
  type _2 = _1# ++
  type _3 = _2# ++

  val _0: _0 = Zero
  val _1: _1 = new Succ[_0]
  val _2: _2 = new Succ[_1]
  val _3: _3 = new Succ[_2]

  sealed trait DepType[N <: Nat] { type T }
  implicit object depType0 extends DepType[_0] { type T = Int }
  implicit object depType1 extends DepType[_1] { type T = String }
  implicit def depType[N <: Nat] = new DepType[Succ[Succ[N]]] { type T = Boolean }

  case class DepPair[N <: Nat, V](x: N, value: V)(implicit depType: DepType[N] { type T = V })

  DepPair(_0, 10)
  DepPair(_1, "a")
  DepPair(_2, true)

  //DepPair(_3, "b") //This does not compile!

}
