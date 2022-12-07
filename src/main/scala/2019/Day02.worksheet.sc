val input = io.Source.fromResource("2019/day-02.txt").getLines().next()
val intcode = input.split(',').map(_.toInt).toVector

def step(intcode: Vector[Int], pc: Int): Option[(Vector[Int], Int)] =
  if intcode(pc) == 99 then None else
    intcode.slice(pc, pc + 4) match
      case Vector(1, x, y, addr) =>
        Some(intcode.updated(addr, intcode(x) + intcode(y)) -> (pc + 4))
      case Vector(2, x, y, addr) =>
        Some(intcode.updated(addr, intcode(x) * intcode(y)) -> (pc + 4))
      case _ => ???

val test = Vector(1,9,10,3,2,3,11,0,99,30,40,50)

step(test, 0)

def run(initialState: Vector[Int]): Vector[Int] =
  LazyList.unfold(initialState -> 0) {
    step(_, _).map { case (state, pc) => state -> (state, pc) }
  }.last

run(Vector(1,0,0,0,99))

val ans1 = run(intcode.updated(1, 12).updated(2, 2))(0)

val ans2 =
  for
    noun <- 0 to 99
    verb <- 0 to 99
    start = intcode.updated(1, noun).updated(2, verb)
    if run(start)(0) == 19690720
  yield
    noun -> verb
