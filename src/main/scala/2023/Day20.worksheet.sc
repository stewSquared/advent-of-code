import aoc.*

val input = io.Source.fromResource("2023/day-20.txt").getLines.toList

sealed trait Module:
  def destinations: List[String]

enum Pulse:
  case High, Low
import Pulse.*

case class FlipFlop(on: Boolean, destinations: List[String]) extends Module

case class Conjunction(memory: Map[String, Pulse], destinations: List[String]) extends Module

case class Broadcaster(destinations: List[String]) extends Module

case class Action(sender: String, pulse: Pulse, dest: String)

val modules = Map.from[String, Module]:
  input.map:
    case s"%$name -> $moduleNames" =>
      val dests = moduleNames.split(", ").toList
      name -> FlipFlop(on = false, dests)
    case s"&$name -> $moduleNames" =>
      val dests = moduleNames.split(", ").toList
      val inputs = input.collect:
        case s"%$send -> $dest" if dest == name => send
        case s"&$send -> $dest" if dest == name => send
        case s"$send -> $dest" if dest == name => send
      name -> Conjunction(inputs.map(i => i -> Low).toMap, dests)
    case s"broadcaster -> $moduleNames" =>
      val dests = moduleNames.split(", ").toList
      "broadcaster" -> Broadcaster(dests)

modules foreach println


import scala.collection.immutable.Queue


type State = Map[String, Module]

val memo = collection.mutable.Map.empty[State, (Int, Int, State)]

def buttonPress(modules: Map[String, Module]): (Int, Int, Map[String, Module]) =
  lazy val calc =
    val start: Queue[Action] = Queue(Action("button", Low, "broadcaster"))

    val steps = LazyList.unfold[(Pulse, State), (Queue[Action], Map[String, Module])](start -> modules):
      case (actionQueue: Queue[Action], moduleStates) =>
        Option.unless(actionQueue.isEmpty):
          val action = actionQueue.head
          println(s"handling $action")
          moduleStates.get(action.dest) match
            case Some(FlipFlop(on, destinations)) if action.pulse == Low =>
              val newActions = actionQueue.tail.enqueueAll:
                destinations.map: dest =>
                  val pulse = if on then Low else High
                  Action(action.dest, pulse, dest)
              val newStates = moduleStates.updated(action.dest, FlipFlop(!on, destinations))
              (action.pulse, newStates) -> (newActions, newStates)
            case Some(FlipFlop(on, destinations)) if action.pulse == High =>
              (action.pulse, moduleStates) -> (actionQueue.tail, moduleStates)
            case Some(Conjunction(memory, destinations)) =>
              val newMemory = memory.updated(action.sender, action.pulse)
              val newActions = actionQueue.tail.enqueueAll:
                destinations.map: dest =>
                  val pulse = if newMemory.values.forall(_ == High) then Low else High
                  Action(action.dest, pulse, dest)
              val newStates = moduleStates.updated(action.dest, Conjunction(newMemory, destinations))
              (action.pulse, newStates) -> (newActions, newStates)
            case Some(Broadcaster(destinations)) =>
              val newActions = actionQueue.tail.enqueueAll:
                destinations.map: dest =>
                  Action(action.dest, action.pulse, dest)
              (action.pulse, moduleStates) -> (newActions, moduleStates)
            case None =>
              (action.pulse, moduleStates) -> (actionQueue.tail, moduleStates)

    val (pulses, _) = steps.unzip
    val finalState = steps.last._2
    val lowCount = pulses.count(_ == Low)
    val highCount = pulses.count(_ == High)
    (lowCount, highCount, finalState)

  memo.getOrElseUpdate(modules, calc)

val (l, h, s) = buttonPress(modules)
l * h

val presses = Iterator.iterate((0, 0, modules)):
  case (lowCount, highCount, moduleStates) =>
    val (newLowCount, newHighCount, newModuleStates) = buttonPress(moduleStates)
    (lowCount + newLowCount, highCount + newHighCount, newModuleStates)

val (low, high, _) = presses.drop(1000).next()
val ans1 = low.toLong * high.toLong

//
// val presses = LazyList.iterate((0, 0, modules)):
//   case (lowCount, highCount, moduleStates) =>
//     val (newLowCount, newHighCount, newModuleStates) = buttonPress(moduleStates)
//     (lowCount + newLowCount, highCount + newHighCount, newModuleStates)

// val (low, high, _) = presses(1000)
// val ans1 = low * high

// modules send pulses, high or low

// flip flops flip when receiving a low
// on -> off and send low
// off -> on and send high
// conjunction have multiple inputs
// they remember what kind of pulse they received last from each input
// the broadcast module sends the same pulse it received last time
// pushing button module sends low pulse to broadcast
// pulses processed in order sent (use a queue)
//



// low pluses times high pulses
//
