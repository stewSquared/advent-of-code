import util.chaining.*

val input = io.Source.fromResource("2024/day-24.txt").getLines.toList

val initWires: State = Map.from[String, Boolean]:
  input.collect: // TODO normalize??
    case s"$name: 1" => name -> true
    case s"$name: 0" => name -> false

sealed trait Gate:
  def a: String
  def b: String
  def swap: Gate = this match
    case AND(a, b) => AND(b, a)
    case OR(a, b) => OR(b, a)
    case XOR(a, b) => XOR(b, a)

case class AND(a: String, b: String) extends Gate
case class OR(a: String, b: String) extends Gate
case class XOR(a: String, b: String) extends Gate

val initGates: Map[String, Gate] = input.collect:
  case s"$a AND $b -> $out" => out -> AND(a, b)
  case s"$a OR $b -> $out" => out -> OR(a, b)
  case s"$a XOR $b -> $out" => out -> XOR(a, b)
.toMap[String, Gate]

initGates.size
// (222! / 216!) / 4! / 2*4
// 222 * 221 * 220 * 219 * 218 * 217 * 216 * 215 / 24 / 8

val zGates: List[(String, Gate)] = initGates.toList.filter:
  case (out, gate) => out.startsWith("z")
.sortBy(_._1).reverse

type State = Map[String, Boolean]

def eval(wire: String, state: State): (Boolean, State) =
  if state.contains(wire) then state(wire) -> state
  else eval(initGates(wire), state)

// for
//   va <- eval(a)
//   vb <- eval(b)
// yield va && vb

def eval(gate: Gate, state: State): (Boolean, State) = gate match
  case AND(a, b) => eval(a, state).pipe:
    case (va, state) => eval(b, state).pipe:
      case (vb, state) => (va && vb) -> state
  case OR(a, b) => eval(a, state).pipe:
    case (va, state) => eval(b, state).pipe:
      case (vb, state) => (va || vb) -> state
  case XOR(a, b) => eval(a, state).pipe:
    case (va, state) => eval(b, state).pipe:
      case (vb, state) => (va ^ vb) -> state

// val (ans1, _) = zGates.foldLeft[(Long, State)](0L, initWires):
//   case ((value, state), (out, gate)) =>
//     val (v, s) = eval(gate, state)
//     ((value << 1) | (if v then 1L else 0L)) -> s

def intState(pre: String, n: Long): Map[String, Boolean] =
  val padding = (0 until 64).map(i => f"$pre${i}%02d" -> false).toMap

  val bits = Iterator.unfold(n): n =>
    Option.when(n != 0):
      val b = n & 1
      (b) -> (n >> 1)

  val binary = bits.zipWithIndex.map:
    case (b, i) => f"$pre${i}%02d" -> (b == 1)
  .toMap

  padding ++ binary

def add(x: Long, y: Long, gates: Map[String, Gate]): Long =
  val initX = intState("x", x)
  val initY = intState("y", y)
  val initWires = initX ++ initY

  val (zValue, _) = zGates.foldLeft[(Long, State)](0L, initWires):
    case ((value, state), (out, gate)) =>
      val (v, s) = eval(gate, state)
      ((value << 1) | (if v then 1L else 0L)) -> s
  zValue

def addBit(x: Long, y: Long, gates: Map[String, Gate], col: Int): Long =
  val initX = intState("x", x)
  val initY = intState("y", y)
  val initWires = initX ++ initY
  val newZgates =
    val f = Set(f"z${col}%02d", f"z${col + 1}%02d")
    zGates.filter:
      case (out, gate) => f.contains(out)

  val (zValue, _) = newZgates.foldLeft[(Long, State)](0L, initWires):
    case ((value, state), (out, gate)) =>
      val (v, s) = eval(gate, state)
      ((value << 1) | (if v then 1L else 0L)) -> s
  zValue

val modGates = collection.mutable.Map.from(initGates)
val swappedLabels = collection.mutable.ListBuffer.empty[String]

def nameOf(gate: Gate): String = modGates.collectFirst:
  case (name, g) if g == gate || g == gate.swap => name
.get

val z00 = initGates("z00")
val c00 = nameOf(AND("x00", "y00"))

val a01 = nameOf(XOR("x01", "y01"))
val b01 = nameOf(AND("x01", "y01"))
val o01 = nameOf(XOR(a01, c00))
o01 == "z01"
val p01 = nameOf(AND(a01, c00))
val c01 = nameOf(OR(b01, p01))

val a02 = nameOf(XOR("x02", "y02"))
val b02 = nameOf(AND("x02", "y02"))
val o02 = nameOf(XOR(a02, c01))
o02 == "z02"
val p02 = nameOf(AND(a02, c01))
val c02 = nameOf(OR(b02, p02))

val a03 = nameOf(XOR("x03", "y03"))
val b03 = nameOf(AND("x03", "y03"))
val o03 = nameOf(XOR(a03, c02))
o03 == "z03"
val p03 = nameOf(AND(a03, c02))
val c03 = nameOf(OR(b03, p03))

val a04 = nameOf(XOR("x04", "y04"))
val b04 = nameOf(AND("x04", "y04"))
val o04 = nameOf(XOR(a04, c03))
o04 == "z04"
val p04 = nameOf(AND(a04, c03))
val c04 = nameOf(OR(b04, p04))

val a05 = nameOf(XOR("x05", "y05"))
val b05 = nameOf(AND("x05", "y05"))
val o05 = nameOf(XOR(a05, c04))
o05 == "z05"
val p05 = nameOf(AND(a05, c04))
val c05 = nameOf(OR(b05, p05))

val a06 = nameOf(XOR("x06", "y06"))
val b06 = nameOf(AND("x06", "y06"))
val o06 = nameOf(XOR(a06, c05))
o06 == "z06"
val p06 = nameOf(AND(a06, c05))
val c06 = nameOf(OR(b06, p06))


val a07 = nameOf(XOR("x07", "y07"))
val b07 = nameOf(AND("x07", "y07"))

val z07fake = modGates("z07")
modGates("z07") = modGates("bjm")
modGates("bjm") = z07fake
swappedLabels += "z07"
swappedLabels += "bjm"

val o07 = nameOf(XOR(a07, c06))
o07 == "z07"
val p07 = nameOf(AND(a07, c06))
val c07 = nameOf(OR(b07, p07))

val a08 = nameOf(XOR("x08", "y08"))
val b08 = nameOf(AND("x08", "y08"))
val o08 = nameOf(XOR(a08, c07))
o08 == "z08"
val p08 = nameOf(AND(a08, c07))
val c08 = nameOf(OR(b08, p08))

val a09 = nameOf(XOR("x09", "y09"))
val b09 = nameOf(AND("x09", "y09"))
val o09 = nameOf(XOR(a09, c08))
o09 == "z09"
val p09 = nameOf(AND(a09, c08))
val c09 = nameOf(OR(b09, p09))

val a10 = nameOf(XOR("x10", "y10"))
val b10 = nameOf(AND("x10", "y10"))
val o10 = nameOf(XOR(a10, c09))
o10 == "z10"
val p10 = nameOf(AND(a10, c09))
val c10 = nameOf(OR(b10, p10))

val a11 = nameOf(XOR("x11", "y11"))
val b11 = nameOf(AND("x11", "y11"))
val o11 = nameOf(XOR(a11, c10))
o11 == "z11"
val p11 = nameOf(AND(a11, c10))
val c11 = nameOf(OR(b11, p11))

val a12 = nameOf(XOR("x12", "y12"))
val b12 = nameOf(AND("x12", "y12"))
val o12 = nameOf(XOR(a12, c11))
o12 == "z12"
val p12 = nameOf(AND(a12, c11))
val c12 = nameOf(OR(b12, p12))

val a13 = nameOf(XOR("x13", "y13"))
val b13 = nameOf(AND("x13", "y13"))

val z13fake = modGates("z13")
val z13label = nameOf(XOR(a13, c12))
swappedLabels += "z13"
swappedLabels += z13label
modGates("z13") = modGates(z13label)
modGates(z13label) = z13fake

val o13 = nameOf(XOR(a13, c12))
o13 == "z13"
val p13 = nameOf(AND(a13, c12))
val c13 = nameOf(OR(b13, p13))

val a14 = nameOf(XOR("x14", "y14"))
val b14 = nameOf(AND("x14", "y14"))
val o14 = nameOf(XOR(a14, c13))
o14 == "z14"
val p14 = nameOf(AND(a14, c13))
val c14 = nameOf(OR(b14, p14))

val a15 = nameOf(XOR("x15", "y15"))
val b15 = nameOf(AND("x15", "y15"))
val o15 = nameOf(XOR(a15, c14))
o15 == "z15"
val p15 = nameOf(AND(a15, c14))
val c15 = nameOf(OR(b15, p15))

val a16 = nameOf(XOR("x16", "y16"))
val b16 = nameOf(AND("x16", "y16"))
val o16 = nameOf(XOR(a16, c15))
o16 == "z16"
val p16 = nameOf(AND(a16, c15))
val c16 = nameOf(OR(b16, p16))

val a17 = nameOf(XOR("x17", "y17"))
val b17 = nameOf(AND("x17", "y17"))
val o17 = nameOf(XOR(a17, c16))
o17 == "z17"
val p17 = nameOf(AND(a17, c16))
val c17 = nameOf(OR(b17, p17))

val a18 = nameOf(XOR("x18", "y18"))

val z18fake = modGates("z18")
val z18label = nameOf(XOR(a18, c17))
swappedLabels += "z18"
swappedLabels += z18label
modGates("z18") = modGates(z18label)
modGates(z18label) = z18fake

val b18 = nameOf(AND("x18", "y18")) // skf
val o18 = nameOf(XOR(a18, c17))
o18 == "z18"
val p18 = nameOf(AND(a18, c17))

val c18 = nameOf(OR(b18, p18)) // rrq

val a19 = nameOf(XOR("x19", "y19"))
val b19 = nameOf(AND("x19", "y19"))

// modGates.filter:
//   case (name, gate) => gate.a == a19 || gate.b == a19

val o19 = nameOf(XOR(a19, c18))
o19 == "z19"
val p19 = nameOf(AND(a19, c18))
val c19 = nameOf(OR(b19, p19))

val a20 = nameOf(XOR("x20", "y20"))
val b20 = nameOf(AND("x20", "y20"))
val o20 = nameOf(XOR(a20, c19))
o20 == "z20"
val p20 = nameOf(AND(a20, c19))
val c20 = nameOf(OR(b20, p20))

val a21 = nameOf(XOR("x21", "y21"))
val b21 = nameOf(AND("x21", "y21"))
val o21 = nameOf(XOR(a21, c20))
o21 == "z21"
val p21 = nameOf(AND(a21, c20))
val c21 = nameOf(OR(b21, p21))

val a22 = nameOf(XOR("x22", "y22"))
val b22 = nameOf(AND("x22", "y22"))
val o22 = nameOf(XOR(a22, c21))
o22 == "z22"
val p22 = nameOf(AND(a22, c21))
val c22 = nameOf(OR(b22, p22))

val a23 = nameOf(XOR("x23", "y23"))
val b23 = nameOf(AND("x23", "y23"))
val o23 = nameOf(XOR(a23, c22))
o23 == "z23"
val p23 = nameOf(AND(a23, c22))
val c23 = nameOf(OR(b23, p23))

val a24 = nameOf(XOR("x24", "y24"))
val b24 = nameOf(AND("x24", "y24"))
val o24 = nameOf(XOR(a24, c23))
o24 == "z24"
val p24 = nameOf(AND(a24, c23))
val c24 = nameOf(OR(b24, p24))

val a25 = nameOf(XOR("x25", "y25"))
val b25 = nameOf(AND("x25", "y25"))
val o25 = nameOf(XOR(a25, c24))
o25 == "z25"
val p25 = nameOf(AND(a25, c24))

nameOf(OR(b25, p25))

// val t25 = modGates("frt")
// modGates("frt") = modGates("qkf")
// modGates("qkf") = t25
// swappedLabels += "qkf"
// swappedLabels += "frt"

swappedLabels.toList
swappedLabels.toList.size

val c25 = nameOf(OR(b25, p25)) // frt
modGates("frt")

nameOf(XOR("x26", "y26"))
modGates("nvr")
modGates("z26")

swappedLabels += "nvr"
swappedLabels += "wkr"
val t26 = modGates("nvr")
modGates("nvr") = modGates("wkr")
modGates("wkr") = t26

val a26 = nameOf(XOR("x26", "y26"))
val b26 = nameOf(AND("x26", "y26"))

modGates.filter:
  case (name, gate) => gate.a == a26 || gate.b == a26


val o26 = nameOf(XOR(a26, c25))
o26 == "z26"
val p26 = nameOf(AND(a26, c25))
val c26 = nameOf(OR(b26, p26))

val a27 = nameOf(XOR("x27", "y27"))
val b27 = nameOf(AND("x27", "y27"))
val o27 = nameOf(XOR(a27, c26))
o27 == "z27"
val p27 = nameOf(AND(a27, c26))
val c27 = nameOf(OR(b27, p27))

val a28 = nameOf(XOR("x28", "y28"))
val b28 = nameOf(AND("x28", "y28"))
val o28 = nameOf(XOR(a28, c27))
o28 == "z28"
val p28 = nameOf(AND(a28, c27))
val c28 = nameOf(OR(b28, p28))

val a29 = nameOf(XOR("x29", "y29"))
val b29 = nameOf(AND("x29", "y29"))
val o29 = nameOf(XOR(a29, c28))
o29 == "z29"
val p29 = nameOf(AND(a29, c28))
val c29 = nameOf(OR(b29, p29))

val a30 = nameOf(XOR("x30", "y30"))
val b30 = nameOf(AND("x30", "y30"))
val o30 = nameOf(XOR(a30, c29))
o30 == "z30"
val p30 = nameOf(AND(a30, c29))
val c30 = nameOf(OR(b30, p30))

val a31 = nameOf(XOR("x31", "y31"))
val b31 = nameOf(AND("x31", "y31"))
val o31 = nameOf(XOR(a31, c30))
o31 == "z31"
val p31 = nameOf(AND(a31, c30))
val c31 = nameOf(OR(b31, p31))

val a32 = nameOf(XOR("x32", "y32"))
val b32 = nameOf(AND("x32", "y32"))
val o32 = nameOf(XOR(a32, c31))
o32 == "z32"
val p32 = nameOf(AND(a32, c31))
val c32 = nameOf(OR(b32, p32))

val a33 = nameOf(XOR("x33", "y33"))
val b33 = nameOf(AND("x33", "y33"))
val o33 = nameOf(XOR(a33, c32))
o33 == "z33"
val p33 = nameOf(AND(a33, c32))
val c33 = nameOf(OR(b33, p33))

val a34 = nameOf(XOR("x34", "y34"))
val b34 = nameOf(AND("x34", "y34"))
val o34 = nameOf(XOR(a34, c33))
o34 == "z34"
val p34 = nameOf(AND(a34, c33))
val c34 = nameOf(OR(b34, p34))

val a35 = nameOf(XOR("x35", "y35"))
val b35 = nameOf(AND("x35", "y35"))
val o35 = nameOf(XOR(a35, c34))
o35 == "z35"
val p35 = nameOf(AND(a35, c34))
val c35 = nameOf(OR(b35, p35))

val a36 = nameOf(XOR("x36", "y36"))
val b36 = nameOf(AND("x36", "y36"))
val o36 = nameOf(XOR(a36, c35))
o36 == "z36"
val p36 = nameOf(AND(a36, c35))
val c36 = nameOf(OR(b36, p36))

val a37 = nameOf(XOR("x37", "y37"))
val b37 = nameOf(AND("x37", "y37"))
val o37 = nameOf(XOR(a37, c36))
o37 == "z37"
val p37 = nameOf(AND(a37, c36))
val c37 = nameOf(OR(b37, p37))

val a38 = nameOf(XOR("x38", "y38"))
val b38 = nameOf(AND("x38", "y38"))
val o38 = nameOf(XOR(a38, c37))
o38 == "z38"
val p38 = nameOf(AND(a38, c37))
val c38 = nameOf(OR(b38, p38))

val a39 = nameOf(XOR("x39", "y39"))
val b39 = nameOf(AND("x39", "y39"))
val o39 = nameOf(XOR(a39, c38))
o39 == "z39"
val p39 = nameOf(AND(a39, c38))
val c39 = nameOf(OR(b39, p39))

val a40 = nameOf(XOR("x40", "y40"))
val b40 = nameOf(AND("x40", "y40"))
val o40 = nameOf(XOR(a40, c39))
o40 == "z40"
val p40 = nameOf(AND(a40, c39))
val c40 = nameOf(OR(b40, p40))

val a41 = nameOf(XOR("x41", "y41"))
val b41 = nameOf(AND("x41", "y41"))
val o41 = nameOf(XOR(a41, c40))
o41 == "z41"
val p41 = nameOf(AND(a41, c40))
val c41 = nameOf(OR(b41, p41))

val a42 = nameOf(XOR("x42", "y42"))
val b42 = nameOf(AND("x42", "y42"))
val o42 = nameOf(XOR(a42, c41))
o42 == "z42"
val p42 = nameOf(AND(a42, c41))
val c42 = nameOf(OR(b42, p42))

val a43 = nameOf(XOR("x43", "y43"))
val b43 = nameOf(AND("x43", "y43"))
val o43 = nameOf(XOR(a43, c42))
o43 == "z43"
val p43 = nameOf(AND(a43, c42))
val c43 = nameOf(OR(b43, p43))

val a44 = nameOf(XOR("x44", "y44"))
val b44 = nameOf(AND("x44", "y44"))
val o44 = nameOf(XOR(a44, c43))
o44 == "z44"
val p44 = nameOf(AND(a44, c43))
val c44 = nameOf(OR(b44, p44))

val ans2 = swappedLabels.toList.sorted.mkString(",")

def meta(i: Int): Unit =
  println(f"""val a${i}%02d = nameOf(XOR("x${i}%02d", "y${i}%02d"))""")
  println(f"""val b${i}%02d = nameOf(AND("x${i}%02d", "y${i}%02d"))""")
  println(f"""val o${i}%02d = nameOf(XOR(a${i}%02d, c${i-1}%02d))""")
  println(f"""o${i}%02d == "z${i}%02d"""")
  println(f"""val p${i}%02d = nameOf(AND(a${i}%02d, c${i-1}%02d))""")
  println(f"""val c${i}%02d = nameOf(OR(b${i}%02d, p${i}%02d))""")
  println()

// for i <- 4 to 44 do meta(i)


// 6 adds 1+1 == 0
// 7 adds 1+1 == 1
// 12 adds 1+1 == 0
// z45 OR
// z18 AND
// z07 OR
