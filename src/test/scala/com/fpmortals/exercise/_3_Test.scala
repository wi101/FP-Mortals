package com.fpmortals.exercise

import com.fpmortals.exercise._3_applicationdesign._
import scalaz.NonEmptyList
import time._
import scalaz._
import Scalaz._
import org.scalatest.{FlatSpec, Matchers}

class Test extends FlatSpec with Matchers {

  import Data._

  "Business Logic" should "generate an initial world view" in {
    val mutable = new Mutable(needsAgents)
    import mutable._
    program.initial shouldBe needsAgents
  }

  it should "remove changed nodes from pending" in {
    val world = WorldView(0, 0, managed, Map(node1 -> time3), Map.empty, time3)
    val mutable = new Mutable(world)
    import mutable._
    val old = world.copy(alive = Map.empty,
      pending = Map(node1 -> time2),
      time = time2)
    program.update(old) shouldBe world
  }

  it should "request agents when needed" in {
    val mutable = new Mutable(needsAgents)
    import mutable._
    val expected = needsAgents.copy(
      pending = Map(node1 -> time1)
    )
    program.act(needsAgents) shouldBe expected
    mutable.stopped shouldBe 0
    mutable.started shouldBe 1
  }

  //We need a var to update started & stopped, we will replace it in the next part using something safer :-)
  class Mutable(state: WorldView) {
    var started, stopped: Int = 0
    private val D: Drone[Id] = new Drone[Id] {
      def getBacklog: Int = state.backlog

      def getAgents: Int = state.agents
    }
    private val M: Machines[Id] = new Machines[Id] {
      def getAlive: Map[MachineNode, Epoch] = state.alive

      def getManaged: NonEmptyList[MachineNode] = state.managed

      def getTime: Epoch = state.time

      def start(node: MachineNode): MachineNode = {
        started += 1; node
      }

      def stop(node: MachineNode): MachineNode = {
        stopped += 1; node
      }
    }
    val program = new DynAgentsModule[Id](D, M)
  }
}

object Data {
  val node1 = MachineNode("1243d1af-828f-4ba3-9fc0-a19d86852b5a")
  val node2 = MachineNode("550c4943-229e-47b0-b6be-3d686c5f013f")
  val managed = NonEmptyList(node1, node2)
  val time1: Epoch = epoch"2017-03-03T18:07:00Z"
  val time2: Epoch = epoch"2017-03-03T18:59:00Z" // +52 mins
  val time3: Epoch = epoch"2017-03-03T19:06:00Z" // +59 mins
  val time4: Epoch = epoch"2017-03-03T23:07:00Z" // +5 hours
  val needsAgents = WorldView(5, 0, managed, Map.empty, Map.empty, time1)
}
