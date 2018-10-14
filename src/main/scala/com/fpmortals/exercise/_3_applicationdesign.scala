package com.fpmortals.exercise

import java.time.Instant

import scalaz._
import Scalaz._

import contextual._
import scala.concurrent.duration._
import scala.util.control.NonFatal

object _3_applicationdesign {

  /**
    * define an Epoch data type and use contextual library to provide a StringContext `epoch` to create an instance of Epoch from a given string
    * @example
    *          val t1: Epoch = epoch"2017-03-03T18:07:00Z"
    *
    */
  object time {
    implicit class EpochMillisStringContext(sc: StringContext) {
      val epoch: Prefix[Epoch, Context, EpochInterpolator.type] =
        Prefix(EpochInterpolator, sc)
    }
    object EpochInterpolator extends Verifier[Epoch] {
      def check(s: String): Either[(Int, String), Epoch] =
        try Right(Epoch(Instant.parse(s).toEpochMilli))
        catch { case NonFatal(_) => Left((0, "not in ISO-8601 format")) }
    }
    final case class Epoch(millis: Long) extends AnyVal {
      def +(d: FiniteDuration): Epoch = Epoch(millis + d.toMillis)
      def -(e: Epoch): FiniteDuration = (millis - e.millis).millis
    }
  }

  /**
    * In FP an algebra replaces an interface in Java
    * It is an abstraction to design a system
    */
  trait Drone[F[_]] {
    def getBacklog: F[Int]
    def getAgents: F[Int]
  }
  import time.Epoch

  final case class MachineNode(id: String)
  trait Machines[F[_]] {
    def getTime: F[Epoch]
    def getManaged: F[NonEmptyList[MachineNode]]
    def getAlive: F[Map[MachineNode, Epoch]]
    def start(node: MachineNode): F[MachineNode]
    def stop(node: MachineNode): F[MachineNode]
  }

  /**
    * Business logic defines the application's behaviour
    * regroup all fields of Drone and Machine, with an additional field `pending` to track unfulfilled requests.
    */
  final case class WorldView(
      backlog: Int,
      agents: Int,
      managed: NonEmptyList[MachineNode],
      alive: Map[MachineNode, Epoch],
      pending: Map[MachineNode, Epoch],
      time: Epoch
  )

  /**
    * The business logic is to run this forever:
    *   state = initial()
    *   while True:
    *     state = update(state)
    *     state = act(state)
    */
  trait DynAgents[F[_]] {
    def initial: F[WorldView]
    def update(old: WorldView): F[WorldView]
    def act(world: WorldView): F[WorldView]
  }

  /**
    * DynAgentsModule depends on DynAgents with a monadic F (F[_] allows us to use `map, `flatMap`, `pure` ...)
    * So we can use those monadic functions in D and M
    */

  /**
    * @note initial, update and act run each algebreic methods sequentially (using flatMap)
    */
  final class DynAgentsModule[F[_]: Monad](D: Drone[F], M: Machines[F])
      extends DynAgents[F] {

    /**
      * initially we put an empty map into pending
      */
    override def initial: F[WorldView] =
      for {
        backlog <- D.getBacklog
        agents <- D.getAgents
        managed <- M.getManaged
        alive <- M.getAlive
        time <- M.getTime
      } yield
        WorldView(backlog,
                  agents,
                  managed,
                  alive,
                  Map.empty[MachineNode, Epoch],
                  time)

    /**
      * refresh the old world view using initial,
      * we remove it from pending if the node has changed its state and if the pending action takes more than 10 minutes
      */
    override def update(old: WorldView): F[WorldView] =
      for {
        snap <- initial
        changed = symdiff(old.alive.keySet, snap.alive.keySet)
        pending = (old.pending -- changed).filterNot {
          case (_, started) => (snap.time - started) >= 10.minutes
        }
        update = snap.copy(pending = pending)

      } yield update

    private def symdiff[T](a: Set[T], b: Set[T]): Set[T] =
      (a union b) -- (a intersect b)
    /**
      * Scenario to implement act
      * 1. Perform only one action per invocation
      *    We return the candidate node that we want to start, only if there is no nodes alive & no pending actions
      * 2. If there is no backlog, we should stop all stale nodes in their 58 minutes and all nodes should have a maximum lifetime of 5 hours
      */
    private object NeedsAgent {
      def unapply(world: WorldView): Option[MachineNode] = world match {
        case WorldView(backlog, 0, managed, alive, pending, _)
            if backlog > 0 && alive.isEmpty && pending.isEmpty =>
          Some(managed.head)
        case _ => None
      }
    }

    private object Stale {
      def unapply(world: WorldView): Option[NonEmptyList[MachineNode]] =
        world match {
          case WorldView(backlog, _, _, alive, pending, time)
              if alive.nonEmpty =>
            (alive -- pending.keys)
              .collect {
                case (n, started)
                    if backlog == 0 && (time - started).toMinutes % 60 >= 58 =>
                  n
                case (n, started) if (time - started) >= 5.hours => n
              }
              .toList
              .toNel
          case _ => None
        }
    }

    /**
      * Schedules a node to be started or stopped
      */
    override def act(world: WorldView): F[WorldView] = world match {
      case NeedsAgent(node) =>
        for {
          _ <- M.start(node)
          update = world.copy(pending = Map(node -> world.time))
        } yield update
      case Stale(nodes) =>
        nodes.foldLeftM(world) { (world, n) =>
          for {
            _ <- M.stop(n)
            update = world.copy(pending = world.pending + (n -> world.time))
          } yield update
        }
      case _ => world.pure[F]
    }

  }

  /**
    * DynAgentsModuleParallel runs the algebraic methods in `initial and `act in parallel
    */

  final class DynAgentsModuleParallel[F[_]: Monad](D: Drone[F], M: Machines[F])
    extends DynAgents[F] {

    override def initial: F[WorldView] =
      (D.getBacklog |@| D.getAgents |@| M.getManaged |@| M.getAlive |@| M.getTime) {
        case (db, da, mm, ma, mt) => WorldView(db, da, mm, ma, Map.empty, mt)
      }

    /**
      *The same behaviour as before
      */
    override def update(old: WorldView): F[WorldView] =
      for {
        snap <- initial
        changed = symdiff(old.alive.keySet, snap.alive.keySet)
        pending = (old.pending -- changed).filterNot {
          case (_, started) => (snap.time - started) >= 10.minutes
        }
        update = snap.copy(pending = pending)

      } yield update

    private def symdiff[T](a: Set[T], b: Set[T]): Set[T] =
      (a union b) -- (a intersect b)
    /**
      * The same as in DynAgentsModule
      */
    private object NeedsAgent {
      def unapply(world: WorldView): Option[MachineNode] = world match {
        case WorldView(backlog, 0, managed, alive, pending, _)
          if backlog > 0 && alive.isEmpty && pending.isEmpty =>
          Some(managed.head)
        case _ => None
      }
    }

    private object Stale {
      def unapply(world: WorldView): Option[NonEmptyList[MachineNode]] =
        world match {
          case WorldView(backlog, _, _, alive, pending, time)
            if alive.nonEmpty =>
            (alive -- pending.keys)
              .collect {
                case (n, started)
                  if backlog == 0 && (time - started).toMinutes % 60 >= 58 =>
                  n
                case (n, started) if (time - started) >= 5.hours => n
              }
              .toList
              .toNel
          case _ => None
        }
    }

    /**
      * Schedules a node to be started (sequentially like in DynAgentsModule, because we will start one node) and schedules *nodes* to be stopped in parallel
      */
    override def act(world: WorldView): F[WorldView] = world match {
      case NeedsAgent(node) =>
        for {
          _ <- M.start(node)
          update = world.copy(pending = Map(node -> world.time))
        } yield update
      case Stale(nodes) =>
        for {
          stopped <- nodes.traverse(M.stop)
          updates = stopped.map(_ -> world.time).toList.toMap
          update = world.copy(pending = world.pending ++ updates)
        } yield update
      case _ => world.pure[F]
    }

  }
}
