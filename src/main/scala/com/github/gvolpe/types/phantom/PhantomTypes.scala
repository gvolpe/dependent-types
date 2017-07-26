package com.github.gvolpe.types.phantom

import com.github.gvolpe.types.phantom.PhantomTypes.Effect._
import com.github.gvolpe.types.phantom.PhantomTypes.Role._

object PhantomTypes {

  trait Effect
  object Effect {
    trait Read extends Effect
    trait Write extends Effect
    trait Delete extends Effect
    trait Update extends Effect
  }

  trait Role
  object Role {
    trait Anonymous extends Role
    trait User extends Role
    trait Admin extends User
  }

  trait File[R <: Role]
  object File {
    def apply[R <: Role] = new File[R] {}
  }

  trait Action[F <: Effect, R <: Role]
  object Action {
    def apply[F <: Effect, R <: Role] = new Action[F, R] {}

    def read[R <: User](db: File[R]) = new Action[Read, R] {}
    def write[R <: User](db: File[R]) = new Action[Write, R] {}
    def delete[R <: Admin](db: File[R]) = new Action[Delete, R] {}
    def update[R <: Admin](db: File[R]) = new Action[Update, R] {}
  }

//  Action.read(File[Anonymous]) // Does not compile!

  Action.read(File[User])
  Action.write(File[User])

//  Action.delete(File[User]) // Does not compile either!

  Action.read(File[Admin])
  Action.write(File[Admin])
  Action.delete(File[Admin])
  Action.update(File[Admin])

}
