val input = io.Source.fromResource("2022/day-19-ex.txt").getLines().toList

case class Materials(ore: Int, clay: Int, obs: Int, geode: Int)
case class Bots(ore: Int, clay: Int, obs: Int, geode: Int)

// Ideas:
// bottom up / memoize
// best build order for a certain ammount of resource
// manually prioritize saving for next level
// ^ better to make more clay?
// could show as pruning two cases that take the same ammount of time,
// but one has less clay
//

case class State(mats: Materials, bots: Bots):

  // def buildOreBot: Option[State] = copy(
  //   mats.copy(mats.ore - bp),
  //   bots.copy(ore)
  // )



  def next(bp: BluePrint, time: Int = 0) =
    // val maxOreToSpendOnOBots

    for
      (oocost, obots) <- (0 to mats.ore by bp.oo).zipWithIndex
      // if obots <= 1
      (cocost, cbots) <- (0 to (mats.ore - oocost) by bp.co).zipWithIndex
      // if obots == 1 ^ (cbots <= 1)
      ((bocost, bccost), bbots) = locally {
        val oc = 0 to (mats.ore - oocost - cocost) by bp.bo
        val cc = 0 to mats.clay by bp.bc
        oc.zip(cc).zipWithIndex.last
      }
      // if (obots ^ cbots) == 1 || bbots <= 1
      ((gocost, gbcost), gbots) = locally {
        val oc = 0 to (mats.ore - oocost - cocost - bocost) by bp.go
        val bc = 0 to mats.obs by bp.gb
        oc.zip(bc).zipWithIndex.last
      }
      if Set(obots, cbots, bbots, gbots).subsetOf(Set(0, 1))
      if List(obots, cbots, bbots, gbots).count(_ == 1) <= 1

      ocost = oocost + cocost + bocost + gocost
      // if mats.ore - ocost < List(bp.oo, bp.co, bp.bo, bp.go).max
      // if obots + bots.ore < 10
      // if cbots + bots.clay < 10
      // if bbots + bots.obs < 10
    // IDEA: get bots in ratio of materials needed
    yield
      assert(obots >= 0, "obots was negative")
      assert(cbots >= 0, "cbots was negative")
      assert(bbots >= 0, "bbots was negative")
      assert(gbots >= 0, "gbots was negative")
      assert(obots <= 1, "made too many obots")
      assert(cbots <= 1, "made too many cbots")
      assert(bbots <= 1, "made too many bbots")
      assert(gbots <= 1, "made too many gbots")
      assert(ocost <= mats.ore)
      assert(bccost <= mats.clay)
      assert(gbcost <= mats.obs)
      State(
        // upper bound these by max we can spend
        Materials( // todo consider assertion
          (bp.maxOre * time).min(mats.ore + bots.ore - ocost),
          (bp.bc * time).min(mats.clay + bots.clay - bccost),
          (bp.gb * time).min(mats.obs + bots.obs - gbcost),
          mats.geode + bots.geode
        ),
        Bots(
          // use a max to collapse some cases
          (bp.maxOre).min(bots.ore + obots),
          (bp.bc).min(bots.clay + cbots),
          (bp.gb).min(bots.obs + bbots),
          bots.geode + gbots
        )
      )

case class BluePrint(oo: Int, co: Int, bo: Int, bc: Int, go: Int, gb: Int):
  val maxOre = oo max co max bo max go


val blueprints = input.map {
  case s"$_ costs $oo ore.$_ costs $co ore.$_ costs $bo ore and $bc clay.$_ costs $go ore and $gb obsidian." =>
    BluePrint(oo.toInt, co.toInt, bo.toInt, bc.toInt, go.toInt, gb.toInt)
}

val start = State(Materials(0, 0, 0, 0), Bots(1, 0, 0, 0))

val bp = blueprints(0)

bp.oo
bp.co

bp.bo
bp.bc

bp.go
bp.gb



// if we want at least 1 geode bot
// then we need at least 1 obs bot for so 7 minutes
// we need minutes / 7 obs bots

// obs bot creation rate * time

// or we need an obs bot creation rate of
// suppose we need 2 for 3.5 mins
//

// then we need at

def sort(states: Seq[State]) =
  states.sortBy(s => (-s.mats.geode, -s.mats.obs, -s.mats.clay, -s.mats.ore))

def productionLimits(state: State, bp: BluePrint, time: Int): Boolean = true
  // && state.bots.ore <= bp.oo + bp.co + bp.bo + bp.go
  // && state.bots.clay < 10
  // && state.bots.obs < 100
  // && state.bots.geode < 100

  // state.bots.ore <= bp.oo + bp.co + bp.bo + bp.go
  // && state.bots.clay < 10
  // && state.bots.obs < 100
  // && state.bots.geode < 100

  // state.mats.ore < 15 && state.mats.clay < 30

def states(bp: BluePrint) = LazyList.iterate(List(start) -> 0) {
  (states, time) =>
    // prioritize by build target
    // and get those top four
    // Is it ever the case that we want to build more ore bots after we get to obsidian
    states
      .filter(productionLimits(_, bp, time))
      .flatMap(_.next(bp, time).toList)
      .distinct -> (time + 1)
      // .groupBy(s => (s.bots, s.mats.geode, s.mats.obs, s.mats.clay)).map {
      //   case (_, states) => states.maxBy(_.mats.ore)
      // }.groupBy(s => (s.bots, s.mats.geode, s.mats.obs, s.mats.ore)).map {
      //   case (_, states) => states.maxBy(_.mats.clay)
      // }.toList
  }.map(_._1)

bp

val ss = states(bp)

ss(10).size
ss(15).size
// ss(20).size
// ss(24).size

// ss(20).size
// ss(15).size
