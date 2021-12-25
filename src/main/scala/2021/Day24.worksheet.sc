// val input = io.Source.fromResource("2021/day-24-1.txt").getLines.toList

// case class Var(name: String, value: Long)

// enum Inst:
//   case Inp(a: Var)
//   case Add(a: Var, b: Var)
//   case

type Instruction = String
type Program = List[Instruction]
type Var = String
type Input = String
type Fields = Map[Var, Long]

case class State(fields: Fields, input: Input):
  def value(arg: String): Long = arg.toLongOption.getOrElse(fields(arg))
  def eval(instruction: String): State = instruction match
    case s"inp $a" =>    copy(fields.updated(a, input.head.asDigit), input = input.tail)
    case s"add $a $b" => copy(fields.updated(a, value(a) + value(b)))
    case s"mul $a $b" => copy(fields.updated(a, value(a) * value(b)))
    case s"div $a $b" => copy(fields.updated(a, value(a) / value(b)))
    case s"mod $a $b" => copy(fields.updated(a, value(a) % value(b)))
    case s"eql $a $b" => copy(fields.updated(a, if value(a) == value(b) then 1L else 0L))

object Fields:
  final val init = Map("x" -> 0L, "y" -> 0L, "z" -> 0L, "w" -> 0L)

object State:
  def interperet(program: List[String], input: Input, init: Fields = Fields.init): State =
    program.foldLeft[State](State(init, input))(_ eval _)

  def runFrom(start: State, program: List[String]): State =
    program.foldLeft[State](start)(_ eval _)

def inputs: Iterator[String] =
  Iterator.iterate(99999999999999L)(_ - 1L).map(_.toString).filterNot(_.contains('0'))

inputs take 3 foreach println


val monad = io.Source.fromResource("2021/day-24-1.txt").getLines.toList

val monadSubroutines: List[Program] =
  monad.grouped(18).toList

assert(monadSubroutines.forall(_.head == "inp w"))
assert(monadSubroutines.forall(_.last == "add z y"))


// State.interperet(monad, "13579246899999")

def randomInputs(seed: Int = 0): Iterator[String] =
  val rand = util.Random(seed)
  // Iterator.continually(rand.between(11_111_111_111_111L, 100_000_000_000_000L))
  Iterator.continually(rand.between(99_999_999_111_111L, 100_000_000_000_000L))
    .map(_.toString).filterNot(_ contains '0')

// randomInputs(0) take 10 foreach println

def validMonadInput(input: Input): Boolean =
  State.interperet(monad, input).fields("z") == 0


// val (valid, invalid) = randomInputs(0).take(100_000).toList.partition(validMonadInput)
// valid.size
// invalid.size
// valid foreach println

// randomInputs(0).take(1_000).map(State.interperet(monad, _)) foreach println

// runMonad("19999999999999")
// runMonad("29999999999999")
// runMonad("39999999999999")
// runMonad("49999999999999")

// runMonad("99999999999996")
// runMonad("99999999999997")
// runMonad("99999999999998")
// runMonad("99999999999999")

// runMonad("99999999999999")
// runMonad("98999999999999")
// runMonad("97999999999999")
// runMonad("96999999999999")

// runMonad("99999999999999")
// runMonad("99999999999999")
// runMonad("99999999999999")
runMonad("99999999999999")


def runMonad(input: Input): Long =
  State.interperet(monad, input).fields("z")

def runMonadSubroutine(i: Int, z: Long, input: Char): Long =
  State.runFrom(
    State(Fields.init.updated("z", z), input.toString),
    monadSubroutines(i)
  ).fields("z")

// runMonadSubroutine(0, 0, '0')

// monadSubroutines(0).mkString("\n")

assert(monadSubroutines.length == 14)

def runAll(input: String): Long =
  assert(input.length == 14)
  input.zipWithIndex.foldLeft(0L){
    case (z, (c, i)) => runMonadSubroutine(i, z, c)
  }

runMonad("99999999999999")
runAll("99999999999999")

def foo(z: Int, x: Int) = (z / 26 * 25 + x) % 26

(1 / 26) * 25
(26 / 26) * 25
(27 / 26) * 25
(52 / 26) * 25
(78 / 26) * 25

foo(53, 0)

// for c <- '0' to '9' do
//   println(runMonadSubroutine(13, 15L, c))

// for
//   c <- '9' to '9'
//   z <- -500 to 1000
//   next = runMonadSubroutine(13, z, c)
//   if next == 1L
// do
//   println(s"z: $z c: $c")

def lastSub(z: Int, w: Int = 9) =
  if w + 10 == z % 26 then 1
  else (z / 26) * 26 + w + 8

// lastSub(45)
// runMonadSubroutine(13, 45, '9')

// lastSub(44)
// runMonadSubroutine(13, 44, '9')

// 9 + 8

// lastSub(45, 8)
// runMonadSubroutine(13, 45, '8')

// runMonadSubroutine(0, 0L, '9')
// runMonadSubroutine(1, 22L, '9')


// for c <- '1' to '9'
// yield runMonadSubroutine(0, 0L, c)

// for c <- '1' to '9'
// yield runMonadSubroutine(1, 22L, c)


// for
//   c <- '1' to '9'
//   z <- 0 to 1000
//   next = runMonadSubroutine(12, z, c)
//   if 37L to 45L contains next
// do
//   println(s"z: $z c: $c")

import util.chaining.*

var z = 0L

// for c <- '9' to '9'
// do z = runMonadSubroutine(0, z, '9').tap(println)

// for c <- '9' to '9'
// do println(runMonadSubroutine(1, z, c))
// z = runMonadSubroutine(1, z, '9')

// for c <- '9' to '9'
// do println(runMonadSubroutine(2, z, c))
// z = runMonadSubroutine(2, z, '7')

// for c <- '9' to '9'
// do println(runMonadSubroutine(3, z, c))
// z = runMonadSubroutine(3, z, '1')


// val digits = '1' to '9'



// runMonadSubroutine(0, 0L, '9')
// for c <- '1' to '9'
// yield runMonadSubroutine(1, 22L, c)


z = 0L
z = runMonadSubroutine(0, z, '6').tap(println)
z = runMonadSubroutine(1, z, '9').tap(println)
z = runMonadSubroutine(2, z, '9').tap(println)
println(z)
z = runMonadSubroutine(3, z, '1').tap(println)
println(z)
z = runMonadSubroutine(4, z, '4').tap(println)
z = runMonadSubroutine(5, z, '9').tap(println)
println(z)
z = runMonadSubroutine(6, z, '9').tap(println)
z = runMonadSubroutine(7, z, '9').tap(println)
z = runMonadSubroutine(8, z, '9').tap(println)
println(z)
z = runMonadSubroutine(9, z, '7').tap(println)
println(z)
z = runMonadSubroutine(10, z, '5').tap(println)
println(z)
z = runMonadSubroutine(11, z, '3').tap(println)
z = runMonadSubroutine(12, z, '6').tap(println)
z = runMonadSubroutine(13, z, '9').tap(println)

runMonadSubroutine(13, 19, '9').tap(println)
lastSub(19)


z = 0L

z = runMonadSubroutine(0, z, '1').tap(println)
z = runMonadSubroutine(1, z, '4').tap(println)
z = runMonadSubroutine(2, z, '9').tap(println)
z = runMonadSubroutine(3, z, '1').tap(println)
z = runMonadSubroutine(4, z, '1').tap(println)
z = runMonadSubroutine(5, z, '6').tap(println)
z = runMonadSubroutine(6, z, '7').tap(println)
z = runMonadSubroutine(7, z, '5').tap(println)
z = runMonadSubroutine(8, z, '3').tap(println)
z = runMonadSubroutine(9, z, '1').tap(println)
z = runMonadSubroutine(10, z, '1').tap(println)
z = runMonadSubroutine(11, z, '1').tap(println)
z = runMonadSubroutine(12, z, '1').tap(println)
z = runMonadSubroutine(13, z, '4').tap(println)


// z = runMonadSubroutine(9, z, '3').tap(println)
// z = runMonadSubroutine(10, z, '6').tap(println)
// z = runMonadSubroutine(11, z, '9').tap(println)
// z = runMonadSubroutine(12, z, '9').tap(println)
// z = runMonadSubroutine(13, z, '9').tap(println)


71 % 26
lastSub(71)
lastSub(45)
lastSub(19)

3 + 3

for
  w <- List(1, 9)
  // w <- List('1', '9')
  z <- 0L to 1_000L
  if w + 13 == z % 26
  // r = runMonadSubroutine(12, z, w)
  // if 37L to 45L contains r
do println(s"z: $z, w: $w")

for
  z <- 0L to 10_000L
  w <- List('1', '9')
  r = runMonadSubroutine(13, z, w)
  if r == 1
do println(s"z: $z, w: $w")








// for
//   c0 <- digits
//   z1 = runMonadSubroutine(0, 0L, c0)
//   if z1 >= 0
//   c1 <- digits
//   z2 = runMonadSubroutine(1, z1, c1)
//   if z2 >= 0
//   c2 <- digits
//   z3 = runMonadSubroutine(1, z2, c2)
//   if z3 >= 0
//   c3 <- digits
//   z4 = runMonadSubroutine(1, z3, c3)
//   if z4 >= 0
//   c4 <- digits
//   z5 = runMonadSubroutine(1, z4, c4)
//   if z5 >= 0
// do
//   println(z5)
//   // println(List(c0,c1,c2,c3,c4).mkString)


// for
//   c <- '1' to '9'
//   z <- -100000L to 100000L
//   res = runMonadSubroutine(3, z, c)
//   if res < 1000
// do println(s"c $c z $z")



// valid.size
// invalid.size

// valid foreach println
// invalid foreach println

// inputs.find()
// def inputs: Iterator[List[Long]] =
//   val digits = (9 to 1 by -1).mkString
//   for _ <- 1 to 5 yield
//     for d <- digits yield

val x = for
  c1 <- "987"
  c2 <- "987"
yield List(c1,c2).mkString


3 + 3

"987".combinations(2) foreach println


// //
