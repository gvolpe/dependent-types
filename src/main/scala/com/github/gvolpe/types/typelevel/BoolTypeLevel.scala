package com.github.gvolpe.types.typelevel

import scala.language.higherKinds

object BoolTypeLevel {

  sealed trait Bool {
    type Not <: Bool
    type && [B <: Bool] <: Bool
    type || [B <: Bool] <: Bool
    type IfElse [C, T <: C, F <: C] <: C
  }

  type True = True.type
  type False = False.type

  object True extends Bool {
    type Not = False
    type && [B <: Bool] = B
    type || [B <: Bool] = True
    type IfElse [C, T <: C, F <: C] = T
  }

  object False extends Bool {
    type Not = True
    type && [B <: Bool] = False
    type || [B <: Bool] = B
    type IfElse [C, T <: C, F <: C] = F
  }

  implicitly[False# Not =:= True]
  implicitly[False# && [True] =:= False]
  implicitly[False# || [True] =:= True]
  implicitly[False# IfElse[Any, Int, String] =:= String]
//  implicitly[True# IfElse[Any, Int, String] =:= String] // This does not compile!

}

object BoolValueLevel {

  sealed trait Bool {
    def not: Bool
    def &&(b: Bool): Bool
    def ||(b : Bool): Bool
    def ifElse[C](t: => C, f: => C): C
  }

  case object True extends Bool {
    override def not = False
    override def &&(b: Bool) = b
    override def ||(b: Bool) = True
    override def ifElse[C](t: => C, f: => C) = t
  }

  case object False extends Bool {
    override def not = True
    override def &&(b: Bool) = False
    override def ||(b: Bool) = b
    override def ifElse[C](t: => C, f: => C) = f
  }

}
