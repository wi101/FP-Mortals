package com.fpmortals.exercise

import scalaz._
import Scalaz._

import scala.concurrent.Future

object _2_forcomprehension {

  /**
    * The for-comprehension is the ideal FP abstraction for sequential programs that interact with the world
    * For-comprehension is a syntax-sugar
    * It defines a *Sequential programs*
    *
    * @example
    *     val a, b, c = Option(1)
    *     for { i <- a ; j <- b ; k <- c } yield (i + j + k)
    *               | | |
    *               | | |
    *               | | |
    *              \     /
    *               -----
    *            a.flatMap {
    *               i => b.flatMap {
    *                j => c.map {
    *                 k => i + j + k }
    *                }
    *               }
    * @example
    *     for {
    *     i <- a
    *     j <- b
    *     ij = i + j
    *     k <- c
    *     } yield (ij + k)
    *          | | |
    *          | | |
    *          | | |
    *         \     /
    *          -----
    *       a.flatMap {
    *         i => b.map {
    *           j => (j, i + j) }.flatMap {
    *             case (j, ij) => c.map {
    *                k => ij + k }
    *             }
    *         }
    *
    * We cannot assign values before any generators `<-` 😔
    *
    * If there is a condition:
    *           @example
    *
    *           for {
    *           i <-a
    *           j <-b if i > j k <-c
    *           } yield (i + j + k)
    *                | | |
    *                | | |
    *                | | |
    *               \     /
    *                -----
    *           a.flatMap {
    *             i => b.withFilter {
    *               j => i > j }.flatMap {
    *                 j => c.map {
    *                   k => i + j + k }}}
    *
    * If there is no yield in for, the compiler will use foreach
    *
    *  For-comprehension compute all computations in yield only if all actions are defined
    *  but it doesn't tell us what went wrong
    */

  /**
    * If we want to call other method in case the first one is failed inside the for-comprehension
    * we can provide this structure
    */
  import scala.concurrent.ExecutionContext.Implicits.global

  def getFromRedis(s: String): Future[Option[String]] = ???

  def getFromSql(s: String): Future[Option[String]] = ???

  for {
    cache <- getFromRedis(???)
    sql <- getFromSql(???)
  } yield cache orElse sql

  //or

  for {
    cache <- getFromRedis(???)
    res <- cache match {
      case Some(_) => Future.successful(cache)
      case None    => getFromSql(???)
    }
  } yield res

  /**
    * WE CANNOT MIX CONTEXTS!!
    * The context must be the same (in this example we use the context Option and then Future)
    * it doesn't compile:
    * found   : scala.concurrent.Future[Int]
    * [error]  required: Option[?]
    *
    * How the compiler can know which Context.flatMap will use! 😏
    */
  //  def op: Option[Int] = Some(1)
  //  def future: Future[Int] = Future(1)
  //  for {
  //    a <- op
  //    b <- future
  //  } yield a * b
  //

  object MonadTransformer {

    /**
      * Monad transformer
      * OptionT
      * EitherT
      * etc
      * Why?
      * @example
      *
      * def getA: Future[Option[Int]] = ...
      * def getB: Future[Option[Int]] = ...
      * for {
      *    a <- getA
      *    b <- getB
      *  } yield a * b
      *            ^ error! a and b are Option
      *
      */
    def getA: OptionT[Future, Int] = ???
    def getB: OptionT[Future, Int] = ???

    val res: OptionT[Future, Int] = for {
      a <- getA
      b <- getB
    } yield a * b

    val origin: Future[Option[Int]] = res.run

    /**
      * From Future & Option to OptionT
      */
    val getC: Future[Int] = ???
    val getCT: OptionT[Future, Int] = getC.liftM[OptionT]

    val getD: Option[Int] = ???
    val getDT: OptionT[Future, Int] = OptionT(getD.pure[Future])

    /**
      * Add DSL for all conversions:
      *  A => OptionT[Future, A]
      *  Option[A] => OptionT[Future, A]
      *  Future[A] => OptionT[Future, A]
      *  Future[Option[A]] => OptionT[Future, A]
      *
      */
    def liftFutureOption[A](f: Future[Option[A]]): OptionT[Future, A] =
      OptionT(f)
    def liftFuture[A](f: Future[A]): OptionT[Future, A] = f.liftM[OptionT]
    def liftOption[A](o: Option[A]): OptionT[Future, A] =
      OptionT(o.pure[Future])
    def lift[A](a: A): OptionT[Future, A] = liftOption(Option(a))

    /***
      * And then you can apply the conversion using those methods with different ways
      */
    val getCTNormal: OptionT[Future, Int] = liftFuture(getC)
    val getCT1: OptionT[Future, Int] = getC.into(liftFuture)
    val getCT2: OptionT[Future, Int] = getC |> liftFuture

    val getDTNormal: OptionT[Future, Int] = liftOption(getD)
    val getDT1: OptionT[Future, Int] = getD.into(liftOption)
    val getDT2: OptionT[Future, Int] = getD |> liftOption

  }
}
