val input = io.Source.fromResource("2022/day-19.txt").getLines().toList

case class Materials(ore: Int, clay: Int, obs: Int, geode: Int):
  def nonNegative = (ore min clay min obs min geode) >= 0

case class Bots(ore: Int, clay: Int, obs: Int, geode: Int)

case class BluePrint(
    oreBotOreCost: Int,
    clayBotOreCost: Int,
    obsBotOreCost: Int,
    clayCost: Int,
    geodeBotOreCost: Int,
    obsCost: Int
):
  val maxOre =
    obsBotOreCost max clayBotOreCost max geodeBotOreCost max oreBotOreCost

case class State(mats: Materials, bots: Bots, bp: BluePrint, time: Int):
  assert(mats.nonNegative, s"have negative materials: $this")
  // assert(time >= 0)

  def collect(n: Int): State = copy(
    time = time - n,
    mats = mats.copy(
      ore = (mats.ore + bots.ore * n),
      clay = (mats.clay + bots.clay * n),
      obs = (mats.obs + bots.obs * n),
      geode = mats.geode + bots.geode * n
    )
  )

  def buildOreBot: State = copy(
    mats.copy(ore = mats.ore - bp.oreBotOreCost),
    bots.copy(ore = bots.ore + 1)
  )

  def queueOreBot: Option[State] =
    lazy val missingOre = (bp.oreBotOreCost - mats.ore).max(0)
    lazy val turnsNeeded = (missingOre + bots.ore - 1) / bots.ore
    Option.when(bots.ore < bp.maxOre && turnsNeeded + 1 < time) {
      collect(turnsNeeded).collect(1).buildOreBot
    }

  def buildClayBot: State = copy(
    mats.copy(ore = mats.ore - bp.clayBotOreCost),
    bots.copy(clay = bots.clay + 1)
  )

  def queueClayBot: Option[State] =
    lazy val missingOre = (bp.clayBotOreCost - mats.ore).max(0)
    lazy val turnsNeeded = (missingOre + bots.ore - 1) / bots.ore
    Option.when(bots.clay < bp.clayCost  && turnsNeeded + 1 < time) {
      collect(turnsNeeded).collect(1).buildClayBot
    }

  def buildObsBot: State = copy(
    mats.copy(
      ore = mats.ore - bp.obsBotOreCost,
      clay = mats.clay - bp.clayCost
    ),
    bots.copy(obs = bots.obs + 1)
  )

  def queueObs: Option[State] =
    lazy val missingOre = (bp.obsBotOreCost - mats.ore).max(0)
    lazy val turnsNeededO = (missingOre + bots.ore - 1) / bots.ore
    lazy val missingClay = (bp.clayCost - mats.clay).max(0)
    lazy val turnsNeededC = (missingClay + bots.clay - 1) / bots.clay
    lazy val turnsNeeded = turnsNeededO max turnsNeededC
    Option.when(
      bots.obs < bp.obsCost && bots.clay >= 1 && turnsNeeded + 1 < time
    ) {
      collect(turnsNeeded).collect(1).buildObsBot
    }

  def buildGeodeBot = copy(
    mats.copy(
      ore = mats.ore - bp.geodeBotOreCost,
      obs = mats.obs - bp.obsCost
    ),
    bots.copy(geode = bots.geode + 1)
  )

  def queueGeode: Option[State] =
    lazy val missingOre = (bp.geodeBotOreCost - mats.ore).max(0)
    lazy val turnsNeededO = (missingOre + bots.ore - 1) / bots.ore
    lazy val missingObs = (bp.obsCost - mats.obs).max(0)
    lazy val turnsNeededB = (missingObs + bots.obs - 1) / bots.obs
    lazy val turnsNeeded = turnsNeededO max turnsNeededB
    Option.when(bots.obs >= 1 && turnsNeeded + 1 < time) {
      collect(turnsNeeded).collect(1).buildGeodeBot
    }

  // this needs to be greatly limited
  // def saveMoney: Option[State] = Some(this.collect)

  def next: List[State] =
    if time <= 0 then List(this)
    else
      val createBot =
        List(queueOreBot, queueClayBot, queueObs, queueGeode).flatten
      if createBot.isEmpty then
        List(this.collect(time))
      else createBot

val blueprints = input.map {
  case s"$_ costs $oo ore.$_ costs $co ore.$_ costs $bo ore and $bc clay.$_ costs $go ore and $gb obsidian." =>
    BluePrint(oo.toInt, co.toInt, bo.toInt, bc.toInt, go.toInt, gb.toInt)
}

def search(start: State) =
  import collection.mutable.{Set, Map, PriorityQueue, Queue}
  val q = Queue(start)
  val finalStates = Set.empty[State]
  while q.nonEmpty do
    val state = q.dequeue()
    val (finished, continuing) = state.next.partition(_.time <= 0)
    finalStates ++= finished
    q.enqueueAll(continuing)

  finalStates.toSet

val ans1 = blueprints
  .map(State(Materials(0, 0, 0, 0), Bots(1, 0, 0, 0), _, 24))
  .zipWithIndex
  .map { (state, i) =>
    search(state).map(_.mats.geode).max * (i + 1)
  }.sum

// val ans2 = blueprints.take(3)
//   .map(State(Materials(0, 0, 0, 0), Bots(1, 0, 0, 0), _, 32))
//   .map(search(_).map(_.mats.geode).max)
  // .product

val bp2 = State(Materials(0, 0, 0, 0), Bots(1, 0, 0, 0), blueprints(1), 32)

search(bp2).map(_.mats.geode).max


// tests

def sort(states: Seq[State]) =
  states.sortBy(s => (-s.mats.geode, -s.mats.obs, -s.mats.clay, -s.mats.ore))

val bp = blueprints(0)

bp.clayCost
bp.obsCost

val start = State(Materials(0, 0, 0, 0), Bots(1, 0, 0, 0), bp, 24)

start.next foreach println

start.next(1).next foreach println

start.queueClayBot.get.queueClayBot.get.queueClayBot.get

// start
// .queueClayBot.get
// .queueClayBot.get
// .queueClayBot.get
// .queueObs.get
// .queueClayBot.get
// .queueObs.get
// .queueGeode.get
// .queueGeode.get
// .collectN(3)
// figure out how to wait out

// search().count(_.time == 0)
search(start).maxBy(_.mats.geode)
search(start).size
// still wrong, but search state much smaller