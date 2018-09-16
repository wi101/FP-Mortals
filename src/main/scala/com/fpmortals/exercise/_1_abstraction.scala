package com.fpmortals.exercise

import scala.concurrent.Future

import scalaz._
import Scalaz._

object _1_abstraction {

  object withoutGenericType {

    trait TerminalSync {
      def read(): String
      def write(t: String): Unit
    }

    trait TerminalAsync {
      def read(): Future[String]
      def write(t: String): Future[Unit]
    }

  }

  object withGenericType {

    /**
      * Terminal is a sum type it can be either Sync or Async
      * so we should think about adding a type constructor to Terminal
      * But Sync has a simple type kind: `*`
      * and Async has a HKT: `* => *` which is Future[_]
      *
      * Trick: Define an identity type 😃
      * type Id[T] = T
      * it seems a type * => * but its kind is actually * 😈
      */
    type Now[T] = T

    sealed trait Terminal[C[_]] {
      def read(): C[String]
      def write(t: String): C[Unit]
    }

    object TerminalSync extends Terminal[Now] {
      def read(): Now[String] /*or String*/ = ???
      def write(t: String): Now[Unit] /*or unit*/ = ???
    }

    object TerminalAsync extends Terminal[Future] {
      def read(): Future[String] = ???
      def write(t: String): Future[Unit] = ???
    }

    /**
      *
      * C is the context
      * We need to implement an execution environment,
      * to be able to use the T inside of C[T]
      * and we need to wrap a value T inside C[T]
      */
    trait Execution[C[_]] {
      def chain[A, B](c: C[A])(f: A => C[B]): C[B]
      def create[B](b: B): C[B]
    }

    /**
      * Now we can do stuffs with a Terminal & Execution
      * Implement a generic method that lets us execute Synchronous and Asynchronous Terminal
      */
    def echo1[C[_]](t: Terminal[C], e: Execution[C]): C[String] =
      e.chain(t.read()) { in: String =>
        e.chain(t.write(in))(_ => e.create(in))
      }

    /**
      * to improve the implementation
      * let's add flatMap & map for C[_]
      * so let's add them using implicit class to give C those methods
      * Let's do that at the companion object of Execution
      * And implement flatMap & map using chain & create so we need an implicit parameter of execution
      */
    object Execution {
      implicit class Ops[A, C[_]](c: C[A]) {
        def flatMap[B](f: A => C[B])(implicit e: Execution[C]): C[B] =
          e.chain(c)(f)
        def map[B](f: A => B)(implicit e: Execution[C]): C[B] =
          e.chain(c)(f andThen e.create)
      }
    }
    import Execution._

    def echo2[C[_]](t: Terminal[C])(implicit e: Execution[C]): C[String] =
      t.read().flatMap { in: String =>
        t.write(in).map { _: Unit =>
          in
        }
      }

    /**
      * We are able now to use the syntax of for-comprehension
      * which is a syntax sugar over nestef flatMap and map.
      */
    def echo3[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
      for {
        in <- t.read()
        _ <- t.write(in)
      } yield in

//    echo3[Future]
//    echo3[Now]
  }

  object ImplUsingScalaz {

    /**
      * C[_] is monadic
      * instead of implementing chain & create, map & flatMap for C[_] we can use Monad[C] in scalaz
      * instead of defining a type Now[T] = T we can use this type alias Id[T] in scalaz
      * we can write sequential code with Monads
      */

    sealed trait Terminal[C[_]] {
      def read(): C[String]
      def write(t: String): C[Unit]
    }

    object TerminalSync extends Terminal[Id] {
      def read(): Id[String] /*or String*/ = ???
      def write(t: String): Id[Unit] /*or unit*/ = ???
    }

    object TerminalAsync extends Terminal[Future] {
      def read(): Future[String] = ???
      def write(t: String): Future[Unit] = ???
    }

    def echo[C[_]: Monad](implicit t: Terminal[C]): C[String] =
      for {
        in <- t.read()
        _ <- t.write(in)
      } yield in

//    echo[Future]
//    echo[Id]
  }

  object usingIO {

    /**
      * IO is a data structure, it isn't running anything
      * It is a definition of TODO program which is lazily evaluated
      * we can map a String and add programs
      * The impure code is evaluated only when we have an io value and we call `io.interpret`
      * @example
      *         val delayed: IO[String] = echo[IO]
      *         delayed.interpret()
      * The IO programs are interpreted once, in the main method at the end of the world
      *
      */
    final class IO[A](val interpret: () => A) {
      def map[B](f: A => B): IO[B] = IO(f(interpret()))
      def flatMap[B](f: A => IO[B]): IO[B] = IO(f(interpret()).interpret())
    }
    object IO {
      def apply[A](a: =>A): IO[A] = new IO(() => a)
    }
    sealed trait Terminal[C[_]] {
      def read(): C[String]
      def write(t: String): C[Unit]
    }

    object TerminalIO extends Terminal[IO] {
      override def read(): IO[String] = IO(io.StdIn.readLine())
      override def write(t: String): IO[Unit] = IO(println(t))
    }
  }

}
