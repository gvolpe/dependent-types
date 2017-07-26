package com.github.gvolpe.types

object AuxType {

  trait Foo[A] {
    type B
    def value: B
  }

  trait Identity[A]

  object Identity {
    implicit def idString = new Identity[String] {}
    implicit def idBoolean = new Identity[Boolean] {}
  }

  object Foo {
    type Aux[A0, B0] = Foo[A0] { type B = B0 }

    implicit def fooInt = new Foo[Int] {
      override type B = String
      override def value = "Hey!"
    }

    implicit def fooString = new Foo[String] {
      override type B = Boolean
      override def value = true
    }

  }

  def foo[T](t: T)(implicit f: Foo[T]): f.B = f.value

  foo(1)
  foo("qwe")

  // This is not possible because the dependent type (f.B) is in the same list of parameters!
//  def fooAux[T](t: T)(implicit f: Foo[T], id: Identity[f.B]): f.B = f.value

  def fooAux[T, R](t: T)(implicit f: Foo.Aux[T, R], id: Identity[R]): R = f.value

  fooAux(2)
  fooAux("asd")

}


