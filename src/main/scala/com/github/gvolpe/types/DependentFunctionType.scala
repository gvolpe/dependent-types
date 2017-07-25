package com.github.gvolpe.types

/**
  * Dependent Function Type, also called Π-Type (Pi Type)
  *
  * Here's the representation in the Idris language:
  *
  * data DepProduct : (a -> Type) -> (P : a -> Type) -> Type where
  *     MakeDepProduct : {a -> Type} -> {P : a -> Type} -> ((x : a) -> P x) -> DepProduct a P
  *
  * depType : Int -> Type
  * depType 0 = Int
  * depType 1 = String
  * depType _ = Bool
  *
  * depFunction : DepProduct Int (\n => depType n)
  * depFunction = MakeDepProduct (\n => case n of
  *                                 0 = 10
  *                                 1 = "aaa"
  *                                 2 = True)
  * */
object DependentFunctionType {

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

  type One    = Zero# ++
  type Two    = One# ++
  type Three  = Two# ++

  val _0: Zero  = Zero
  val _1: One   = new Succ[Zero]
  val _2: Two   = new Succ[One]
  val _3: Three = new Succ[Two]

  sealed trait DepType[N <: Nat] {
    type T
    def apply(x: N): T
  }

  implicit object depType0 extends DepType[Zero] {
    type T = Int
    override def apply(x: Zero) = 10
  }

  implicit object depType1 extends DepType[One] {
    type T = String
    override def apply(x: One) = "abc"
  }

  implicit def depType[N <: Nat] = new DepType[Succ[Succ[N]]] {
    type T = Boolean
    override def apply(x: Succ[Succ[N]]) = true
  }

  object DepFunction {
    def apply[N <: Nat](x: N)(implicit depType: DepType[N]): depType.T = depType(x)
  }

  val x: Int = DepFunction(_0)
  val y: String = DepFunction(_1)
  val z: Boolean = DepFunction(_2)

  //val t: Boolean = DepFunction(_1) // This does not compile!
}
