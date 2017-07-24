package com.github.gvolpe.types

object PathDependentTypes {

  class A {
    class B {
      class C {
        class D
      }
    }
  }

  val a1: A = new A
  val a2: A = new A

  val b1: a1.B = new a1.B
  val b2: a2.B = new a2.B

  // This is not possible because the type depends on the current instance
  // val b3: a2.B = new a1.B
  // val b4: a1.B = new a2.B

  // Here using type projection
  val b5: A#B = new a1.B
  val b6: A#B = new a2.B

  val c: A#B#C = new b1.C
  val d: A#B#C#D = new c.D

}
