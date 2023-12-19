import aoc.*

val input = io.Source.fromResource("2023/day-19.txt").getLines

enum Category:
  case X, M, A, S

sealed trait Rule

sealed trait Action extends Rule:
  def terminal: Boolean = this match
    case Send(_) => false
    case _ => true

case object Accept extends Action
case object Reject extends Action
case class Send(name: String) extends Action

sealed trait Condition:
  def category: Category

  def applicable(part: Part): Boolean =
    this match
      case LT(category, value) => part.category(category) < value
      case GT(category, value) => part.category(category) > value

  def matching(category: Category): Interval[Int] =
    this match
      case LT(c, v) if c == category => Interval(1 until v)
      case GT(c, v) if c == category => Interval(v + 1 to 4000)
      case _ => Interval(1 to 4000)

  def nonMatching(category: Category): Interval[Int] =
    this match
      case LT(c, v) if c == category => Interval(v to 4000)
      case GT(c, v) if c == category => Interval(1 to v)
      case _ => Interval(1 to 4000)


case class LT(category: Category, value: Int) extends Condition
case class GT(category: Category, value: Int) extends Condition

object Condition:
  def fromString(s: String) = s match
    case s"$c<$v" => LT(Category.valueOf(c.toUpperCase), v.toInt)
    case s"$c>$v" => GT(Category.valueOf(c.toUpperCase), v.toInt)

case class Conditional(conditions: Condition, action: Action) extends Rule

case object Action:
  def fromString(s: String) = s match
    case "A" => Accept
    case "R" => Reject
    case s => Send(s)

case class Part(x: Int, m: Int, a: Int, s: Int):
  def category(category: Category) = category match
    case Category.X => x
    case Category.M => m
    case Category.A => a
    case Category.S => s

  def ratingNumber: Int = x + m + a + s

object Part:
  def fromString(s: String) = s match
    case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toInt, m.toInt, a.toInt, s.toInt)

val workflows = Map.from[String, List[Rule]]:
  val lines = input.takeWhile(_.nonEmpty)
  lines.map[(String, List[Rule])]:
    case s"$name{$rawRules}" =>
      val rules = rawRules.split(",").toList.map:
        case s"$condition:$action" =>
          Conditional(Condition.fromString(condition), Action.fromString(action))
        case action => Action.fromString(action)
      name -> rules.toList

val parts: List[Part] = input.map(Part.fromString).toList

def applyWorkflow(part: Part, name: String): Action =
  val action = workflows(name).collectFirst:
    case Conditional(condition, action) if condition.applicable(part) => action
    case action: Action => action
  action.get

def process(part: Part) =
  LazyList.unfold[Action, Action](Send("in")):
    case Send(name) =>
      val action = applyWorkflow(part, name)
      Some(action -> action)
    case Accept => None
    case Reject => None

def accepted(part: Part) = process(part).contains(Accept)

val ans1 = parts.filter(accepted).map(_.ratingNumber).sum

case class Ranges(
  x: List[Interval[Int]],
  m: List[Interval[Int]],
  a: List[Interval[Int]],
  s: List[Interval[Int]]
):
  override def toString =
    s"x: ${x.mkString(",")} m: ${m.mkString(",")} a: ${a.mkString(",")} s: ${s.mkString(",")}"

  def normalize: Ranges = copy(
    x = x.sortBy(_.min).distinct,
    m = m.sortBy(_.min).distinct,
    a = a.sortBy(_.min).distinct,
    s = s.sortBy(_.min).distinct
  )

  def combinations =
    val xn = x.distinct.map(_.size).sum.toLong
    val mn = m.distinct.map(_.size).sum.toLong
    val an = a.distinct.map(_.size).sum.toLong
    val sn = s.distinct.map(_.size).sum.toLong
    xn * mn * an * sn

  def filterBy(rule: Rule): Ranges = rule match
    case Conditional(condition, _) =>
      condition.category match
        case Category.X =>
          val range = condition.matching(Category.X)
          copy(x = x.flatMap(_.intersect(range)))
        case Category.M =>
          val range = condition.matching(Category.M)
          copy(m = m.flatMap(_.intersect(range)))
        case Category.A =>
          val range = condition.matching(Category.A)
          copy(a = a.flatMap(_.intersect(range)))
        case Category.S =>
          val range = condition.matching(Category.S)
          copy(s = s.flatMap(_.intersect(range)))
    case action: Action => this

  def filterNotBy(rule: Rule): Ranges = rule match
    case Conditional(condition, _) =>
      condition.category match
        case Category.X =>
          val range = condition.nonMatching(Category.X)
          copy(x = x.flatMap(_.intersect(range)))
        case Category.M =>
          val range = condition.nonMatching(Category.M)
          copy(m = m.flatMap(_.intersect(range)))
        case Category.A =>
          val range = condition.nonMatching(Category.A)
          copy(a = a.flatMap(_.intersect(range)))
        case Category.S =>
          val range = condition.nonMatching(Category.S)
          copy(s = s.flatMap(_.intersect(range)))
    case action: Action => this

  def intersect(other: Ranges): Ranges =
    copy(
      x = x.flatMap: interval =>
        other.x.flatMap(interval.intersect)
      .distinct,
      m = m.flatMap: interval =>
        other.m.flatMap(interval.intersect)
      .distinct,
      a = a.flatMap: interval =>
        other.a.flatMap(interval.intersect)
      .distinct,
      s = s.flatMap: interval =>
        other.s.flatMap(interval.intersect)
      .distinct
    )

object Ranges:
  val full = Ranges(
    x = List(Interval(1 to 4000)),
    m = List(Interval(1 to 4000)),
    a = List(Interval(1 to 4000)),
    s = List(Interval(1 to 4000))
  )

def actionRanges(from: String): List[(Action, Ranges)] =
  val rules = workflows(from)
  rules.inits.drop(1).zip(rules.reverse).toList.map:
    case (mustFail, mustPass) =>
      val passedThrough = mustFail
        .foldLeft(Ranges.full)(_ filterNotBy _)
        .filterBy(mustPass)
      mustPass match
        case a: Action => a -> passedThrough
        case Conditional(_, a) => a -> passedThrough

def search(from: String, incoming: Ranges): Long =
  println(s"from $from with $incoming")
  actionRanges(from).map:
    case (action, ranges) => action -> ranges.intersect(incoming)
  .tapEach(ar => println(s"  $ar"))
  .map:
    case (Accept, ranges) =>
      println(s"accepting ${ranges.combinations}")
      ranges.combinations
    case (Reject, ranges) => 0L
    case (Send(to), ranges) => search(to, ranges)
  .sum

val ans2 = search("in", Ranges.full)
