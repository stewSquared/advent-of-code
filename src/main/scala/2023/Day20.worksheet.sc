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

import scala.collection.immutable.Queue

type State = Map[String, Module]

val memo = collection.mutable.Map.empty[State, (Int, Int, State, List[Pulse])]

def buttonPress(modules: Map[String, Module]): (Int, Int, Map[String, Module], List[Action]) =
  lazy val calc =
    val start: Queue[Action] = Queue(Action("button", Low, "broadcaster"))

    val steps = LazyList.unfold[(Action, State), (Queue[Action], Map[String, Module])](start -> modules):
      case (actionQueue: Queue[Action], moduleStates) =>
        Option.unless(actionQueue.isEmpty):
          val action = actionQueue.head
          // println(s"handling $action")
          moduleStates.get(action.dest) match
            case Some(FlipFlop(on, destinations)) if action.pulse == Low =>
              val newActions = actionQueue.tail.enqueueAll:
                destinations.map: dest =>
                  val pulse = if on then Low else High
                  Action(action.dest, pulse, dest)
              val newStates = moduleStates.updated(action.dest, FlipFlop(!on, destinations))
              (action, newStates) -> (newActions, newStates)
            case Some(FlipFlop(on, destinations)) if action.pulse == High =>
              (action, moduleStates) -> (actionQueue.tail, moduleStates)
            case Some(Conjunction(memory, destinations)) =>
              val newMemory = memory.updated(action.sender, action.pulse)
              val newActions = actionQueue.tail.enqueueAll:
                destinations.map: dest =>
                  val pulse = if newMemory.values.forall(_ == High) then Low else High
                  Action(action.dest, pulse, dest)
              val newStates = moduleStates.updated(action.dest, Conjunction(newMemory, destinations))
              (action, newStates) -> (newActions, newStates)
            case Some(Broadcaster(destinations)) =>
              val newActions = actionQueue.tail.enqueueAll:
                destinations.map: dest =>
                  Action(action.dest, action.pulse, dest)
              (action, moduleStates) -> (newActions, moduleStates)
            case None => // rx
              (action, moduleStates) -> (actionQueue.tail, moduleStates)

    val (actions, _) = steps.unzip
    val pulses = actions.map(_.pulse)
    val finalState = steps.last._2
    val lowCount = pulses.count(_ == Low)
    val highCount = pulses.count(_ == High)
    val rxPulses = actions.collect:
      case Action(_, pulse, dest) if dest == "rx" => pulse
    .toList
    (lowCount, highCount, finalState, actions.toList)

  // if (memo.contains(modules)) {
  //   println("memo hit")
  // }
  // memo.getOrElseUpdate(modules, calc)
  calc



// val (l, h, s) = buttonPress(modules)
// l * h

val presses = LazyList.iterate((0, 0, modules, List.empty[Action])):
  case (lowCount, highCount, moduleStates, _) =>
    val (newLowCount, newHighCount, newModuleStates, rxPulses) = buttonPress(moduleStates)
    (lowCount + newLowCount, highCount + newHighCount, newModuleStates, rxPulses)

presses(0)._4

presses(1)._4
presses(2)._4
presses(3)._4
presses(4)._4
presses(5)._4
presses(6)._4
presses(7)._4

presses(1)._3 foreach println
presses(2)._3 foreach println
presses(3)._3 foreach println
presses(4)._3 foreach println
presses(5)._3 foreach println
presses(6)._3 foreach println
presses(7)._3 foreach println


presses(1)._4.filter(_.dest == "rx").groupMap(_.sender)(_._2) foreach println
presses(2)._4.filter(_.dest == "rx").groupMap(_.sender)(_._2) foreach println
presses(3)._4.filter(_.dest == "rx").groupMap(_.sender)(_._2) foreach println
presses(4)._4.filter(_.dest == "rx").groupMap(_.sender)(_._2) foreach println
presses(5)._4.filter(_.dest == "rx").groupMap(_.sender)(_._2) foreach println
presses(6)._4.filter(_.dest == "rx").groupMap(_.sender)(_._2) foreach println
presses(7)._4.filter(_.dest == "rx").groupMap(_.sender)(_._2) foreach println

presses(0)._4.filter(_.dest == "kl").groupMap(_.sender)(_._2) foreach println
presses(1)._4.filter(_.dest == "kl").groupMap(_.sender)(_._2) foreach println
presses(2)._4.filter(_.dest == "kl").groupMap(_.sender)(_._2) foreach println
presses(3)._4.filter(_.dest == "kl").groupMap(_.sender)(_._2) foreach println
presses(4)._4.filter(_.dest == "kl").groupMap(_.sender)(_._2) foreach println
presses(5)._4.filter(_.dest == "kl").groupMap(_.sender)(_._2) foreach println
presses(6)._4.filter(_.dest == "kl").groupMap(_.sender)(_._2) foreach println
presses(7)._4.filter(_.dest == "kl").groupMap(_.sender)(_._2) foreach println



// 000010101101
// qf -> zc

val qfBits = "sg ts tc cl pj rd mn mg lf jz xr jt".split(' ').toList
qfBits.map(b => modules.filter(_._2.destinations.contains(b)).keys) foreach println
Integer.parseInt("000010101101", 2)

modules("broadcaster").destinations

// 1011001

val gvBits = "gx lb gk vk qh tz lq ss kx ps mz gz".split(' ').toList
gvBits.map(b => modules.filter(_._2.destinations.contains(b)).keys) foreach println

val rcBits = "vj, fl, dd, vf, cg, fb, kq, nf, gf, ld, nz, dh".split(", ").reverse.toList
rcBits.map(b => modules.filter(_._2.destinations.contains(b)).keys) foreach println

val llBits = "fh vl vg tx zh zv mx dm qd mr pd sq".split(' ').toList.reverse
llBits.map(b => modules.filter(_._2.destinations.contains(b)).keys) foreach println

2 + 2

val qfBins = presses.map: ps =>
  val bits = qfBits.map: bit =>
    ps._3(bit) match
      case FlipFlop(on, _) => if on then 1 else 0
  bits.mkString

def bins(bitNames: List[String]) = presses.map: ps =>
  val bits = bitNames.map: bit =>
    ps._3(bit) match
      case FlipFlop(on, _) => if on then 1 else 0
  bits.mkString

bins(llBits).map(Integer.parseInt(_, 2)).zipWithIndex.find:
  case (bin, i) => bin != i

bins(qfBits).map(Integer.parseInt(_, 2)).zipWithIndex.find:
  case (bin, i) => bin != i

bins(rcBits).map(Integer.parseInt(_, 2)).zipWithIndex.find:
  case (bin, i) => bin != i

bins(gvBits).map(Integer.parseInt(_, 2)).zipWithIndex.find:
  case (bin, i) => bin != i


// 3931 3923 3767 4007
165 + 3931
329 + 3923

List(3931, 3923, 3767, 4007).map(_.toLong).product

val ll = Integer.parseInt("000010100101", 2)
val rc = Integer.parseInt("000101001001", 2)
val gv = Integer.parseInt("000001011001", 2)
val qf = Integer.parseInt("000010101101", 2)


qfBins.drop(3920).take(10).map: bin =>
  bin -> Integer.parseInt(bin, 2)
.foreach(println)

qfBins.drop(3920 * 2).take(10).map: bin =>
  bin -> Integer.parseInt(bin, 2)
.foreach(println)


2 + 2
// &mk needs high
// - &ll (nine flip flops)
//   - qd, pd, mr, mx, fh, zh, vl, sq, tx
// &fp
// - &rc
//   - nine flip flops
// &xt
// - &gv
//   - nine flip flops
// &zc
// - &qf
//   - eight flip flops

val intoll = modules.filter(_._2.destinations.contains("ll")).keys
val intorc = modules.filter(_._2.destinations.contains("rc")).keys
val intogv = modules.filter(_._2.destinations.contains("gv")).keys
val intoqf = modules.filter(_._2.destinations.contains("qf")).keys

// modules.(rc)

Integer.parseInt("000010100101", 2)
Integer.parseInt("000101001001", 2)
Integer.parseInt("000001011001", 2)
Integer.parseInt("000010101101", 2)

2213.toBinaryString
2213.toBinaryString

0
0 + 1
1 + 2213
2214 + 1
2214 + 2213
(2214 + 2213).toBinaryString

Integer.parseInt("111111111111", 2)
Iterator.iterate(2214)(_ + 2214).map(_.toBinaryString)
  .take(100) foreach println

"000000000000"
"000000000001"
"100010100100"
"100010100110"

"100010100110"
"100010100111"
"100010100101"
"000101001111"

"100010100101"


"100010100101"

"111111111111"
"100010100101"

"111111111110"

Integer.parseInt("111111111111", 2)



List(
//               "111111111111"
Integer.parseInt("100010100101", 2),
Integer.parseInt("000101001001", 2),
Integer.parseInt("000001011001", 2),
Integer.parseInt("000010101101", 2),
).reduce(_ | _)

// ans2.toBinaryString

List(
Integer.parseInt("100010100101", 2).toLong,
Integer.parseInt("000101001001", 2).toLong,
Integer.parseInt("000001011001", 2).toLong,
Integer.parseInt("000010101101", 2).toLong,
).product







val zeroBits = modules("broadcaster").destinations
zeroBits.map: name =>
  val bitModuleNames = LazyList.unfold(name): n =>
    val dests = modules(n).destinations
    val next = dests.map(n => n -> modules).collectFirst:
      case (n, ff: FlipFlop) => n
    .getOrElse(dests.head)
    Some(n -> next)

  val bits = bitModuleNames.takeWhile: n =>
    modules(n).match
      case FlipFlop(on, destinations) => true
      case _ => false
  .map: n =>
    val c = modules.filter(_._2.destinations.contains(n)).collectFirst:
      case (_, c: Conjunction) => c
    if c.isEmpty then 0 else 1

  println(bits.toList)

  Integer.parseInt(bits.mkString.reverse, 2)

println(intoll)
println(intorc)
println(intogv)
println(intoqf)

// intoll.flatMap(m => modules.filter(_._2.destinations.contains(m)).keys)

intoll.toList.map(m => (m, modules(m)) -> modules.filter(_._2.destinations.contains(m)).keys.map(n => n -> modules(n))) foreach println
intorc.toList.map(m => (m, modules(m)) -> modules.filter(_._2.destinations.contains(m)).keys.map(n => n -> modules(n))) foreach println
intogv.toList.map(m => (m, modules(m)) -> modules.filter(_._2.destinations.contains(m)).keys.map(n => n -> modules(n))) foreach println
intoqf.toList.map(m => (m, modules(m)) -> modules.filter(_._2.destinations.contains(m)).keys.map(n => n -> modules(n))) foreach println

List(intoll, intorc, intogv, intoqf).map(_.toSet).reduce(_ intersect _)

modules("broadcaster").destinations.toList.map(m => m -> modules.filter(_._2.destinations.contains(m)).keys.map(modules)) foreach println

9 * 3 + 8
4
4
1

modules.size
9 * 3 + 8 + 4 + 4 + 1


List(intoll, intorc, intogv, intoqf).combinations(2).exists:
  case List(a, b) => a.toSet.intersect(b.toSet).nonEmpty


// presses(0)
// presses(1)

// modules.collect:
//   case (name, module) if module.destinations.contains("rx") => name



// val presses = Iterator.iterate((0, 0, modules, List.empty[Pulse])):
//   case (lowCount, highCount, moduleStates, _) =>
//     val (newLowCount, newHighCount, newModuleStates, rxPulses) = buttonPress(moduleStates)
//     (lowCount + newLowCount, highCount + newHighCount, newModuleStates, rxPulses)

// val (low, high, _, rx) = presses.drop(1000).next()
// val ans1 = low.toLong * high.toLong
// println(rx)

// val ans2 = presses.indexWhere:
//   case (_, _, _, rxPulses) => rxPulses == List(Low)


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
