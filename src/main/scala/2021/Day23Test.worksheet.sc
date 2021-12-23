import `2021`.`23`.*
import io.Source
import util.Using
import collection.mutable.PriorityQueue
import Pos.*

val initial = Using(Source.fromResource("2021/day-23-1.txt"))(Burrow.fromSource).get
// Note: currently too slow to run part 2 in the worksheet (150s)
// use `sbt "runMain 2021.23.day23"`

var visiting = initial
val toVisit = PriorityQueue(initial)
while !visiting.organized do
  visiting.nextStates.foreach(toVisit.enqueue(_))
  visiting = toVisit.dequeue()

val ans1 = visiting.cost
println(visiting.show)

// tests

var s = initial

def next(move: Move): Unit =
  s(move).foreach { b =>
    println(b.show)
    s = b
  }

initial.extraSteps(Amph.A, Move.Out(D,L1))

next(Move.Out(C,L1))
println(s.cost)

println(s.show)
next(Move.Out(D,R1))
next(Move.Out(B,L1))
next(Move.Out(B,L1))



// test
