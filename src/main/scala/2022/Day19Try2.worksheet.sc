val input = io.Source.fromResource("2022/day-19-ex.txt").getLines().toList

case class Materials(ore: Int, clay: Int, obs: Int, geode: Int):
  def nonNegative = (ore min clay min obs min geode) >= 0

case class Bots(ore: Int, clay: Int, obs: Int, geode: Int)

case class BluePrint(
    oreCostO: Int,
    oreCostC: Int,
    oreCostB: Int,
    clayCost: Int,
    oreCostG: Int,
    obsCost: Int
):
  val maxOre = oreCostB max oreCostC max oreCostG max oreCostO

case class State(mats: Materials, bots: Bots, bp: BluePrint, time: Int):
  assert(time >= 0)

  def collect: State = copy(
    time = time - 1,
    mats = mats.copy(
      ore = (mats.ore + bots.ore).min(bp.maxOre * time),
      clay = (mats.clay + bots.clay).min(bp.clayCost * time),
      obs = (mats.obs + bots.obs).min(bp.obsCost * time),
      geode = mats.geode + bots.geode
    )
  )

  def createOre: Option[State] = Option.when(
    bots.ore <= bp.maxOre && mats.ore >= bp.oreCostO
  ) {
    collect.copy(
      mats.copy(ore = mats.ore - bp.oreCostO),
      bots.copy(ore = bots.ore + 1)
    )
  }

  // don't spend ore to create something, if you could have spent it last turn

  def createClay: Option[State] = Option.when(
    bots.clay <= bp.clayCost && mats.ore >= bp.oreCostC
  ) {
    collect.copy(
      mats.copy(ore = mats.ore - bp.oreCostC),
      bots.copy(clay = bots.clay + 1)
    )
  }

  def createObs: Option[State] = Option.when(
    bots.obs <= bp.obsCost && mats.ore >= bp.oreCostB && mats.clay >= bp.clayCost
  ) {
    collect.copy(
      mats.copy(
        ore = mats.ore - bp.oreCostB,
        clay = mats.clay - bp.clayCost
      ),
      bots.copy(obs = bots.obs + 1)
    )
  }

  def createGeode: Option[State] = Option.when(
    mats.ore >= bp.oreCostG && mats.obs >= bp.obsCost
  ) {
    collect.copy(
      mats.copy(
        ore = mats.ore - bp.oreCostG,
        obs = mats.obs - bp.obsCost
      ),
      bots.copy(geode = bots.geode + 1)
    )
  }

  // this needs to be greatly limited
  def saveMoney: Option[State] = Some(this.collect)

  def next: List[State] =
    val createBot =
      List(createOre, createClay, createObs, createGeode, saveMoney).flatten
    if createBot.isEmpty then
      List(this.collect) // < currently dead if we use saveMoney
    else createBot

val blueprints = input.map {
  case s"$_ costs $oo ore.$_ costs $co ore.$_ costs $bo ore and $bc clay.$_ costs $go ore and $gb obsidian." =>
    BluePrint(oo.toInt, co.toInt, bo.toInt, bc.toInt, go.toInt, gb.toInt)
}

//

def sort(states: Seq[State]) =
  states.sortBy(s => (-s.mats.geode, -s.mats.obs, -s.mats.clay, -s.mats.ore))

val bp = blueprints(0)

bp.clayCost
bp.obsCost

val start = State(Materials(0, 0, 0, 0), Bots(1, 0, 0, 0), bp, 24)

start.next foreach println

val ss =
  LazyList.iterate(List(start))(states => states.flatMap(_.next).distinct)

ss(0)
ss(1)
ss(2)
ss(3)
ss(4)

ss(2)
ss(3) foreach println

// now if you want more complicated,
// then you can account for the fact that
//  X ore robots will produce X ore,
// and you will only use atmost maxOre of it in a turn,
// then you only need to stock (maxOre - X) per turn ( something like that )

bp.oreCostC
bp.oreCostO
bp.oreCostB

ss(10).size
ss(15).size
// ss(20).size
// ss(24).size // 35043
// still pruning too much somehow
// don't have enough geodes
// sort(ss(24)) foreach println

// ss(15).size
