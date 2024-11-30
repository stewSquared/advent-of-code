import aoc.*

val input = io.Source.fromResource("2023/day-19.txt").getLines

enum Category:
  case X, M, A, S

sealed trait Rule
enum Action extends Rule:
  case Accept, Reject
  case Send(to: String)
import Action.*

sealed trait Condition:
  def category: Category

  def applicable(part: Part): Boolean =
    this match
      case LT(category, value) => part(category) < value
      case GT(category, value) => part(category) > value

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

type Part = Map[Category, Int]

object Part:
  def fromString(s: String) = s match
    case s"{x=$x,m=$m,a=$a,s=$s}" => apply(x.toInt, m.toInt, a.toInt, s.toInt)

  def apply(x: Int, m: Int, a: Int, s: Int): Part =
    Map(
      Category.X -> x,
      Category.M -> m,
      Category.A -> a,
      Category.S -> s
    )

extension (part: Part)
  def ratingNumber: Int = Category.values.map(part).sum

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

type Ranges = Map[Category, List[Interval[Int]]]

extension (ranges: Ranges)
  def show =
    ranges.map: (category, intervals) =>
      s"$category: ${intervals.mkString(",")}"
    .mkString(" ")

  def normalize: Ranges =
    ranges.view.mapValues(_.sortBy(_.min).distinct).toMap

  def combinations =
    ranges.values.map(_.map(_.size).sum.toLong).product

  def filterBy(rule: Rule): Ranges = rule match
    case Conditional(condition, _) => ranges.map:
      case (cat, cRanges) if cat == condition.category =>
        val filter = condition.matching(condition.category)
        cat -> cRanges.flatMap(_.intersect(filter))
      case others => others
    case action: Action => ranges

  def filterNotBy(rule: Rule): Ranges = rule match
    case Conditional(condition, _) => ranges.map:
      case (cat, cRanges) if cat == condition.category =>
        val filter = condition.nonMatching(cat)
        cat -> cRanges.flatMap(_.intersect(filter))
      case others => others
    case action: Action => ranges

  def intersect(other: Ranges): Ranges =
    ranges.map:
      case (cat, intervals) =>
        cat -> intervals.flatMap(n => other(cat).flatMap(n.intersect))

object Ranges:
  val full = Map(
    Category.X -> List(Interval(1 to 4000)),
    Category.M -> List(Interval(1 to 4000)),
    Category.A -> List(Interval(1 to 4000)),
    Category.S -> List(Interval(1 to 4000))
  )

def actionRanges(from: String): List[(Action, Ranges)] =
  val rules = workflows(from)
  rules.inits.drop(1).zip(rules.reverse).toList.map:
    case (mustFail, mustPass) =>
      val passedThrough = mustFail
        .foldLeft(Ranges.full)(_.filterNotBy(_))
        .filterBy(mustPass)
      mustPass match
        case a: Action => a -> passedThrough
        case Conditional(_, a) => a -> passedThrough

def search(from: String, incoming: Ranges): Long =
  actionRanges(from).map:
    case (action, ranges) => action -> ranges.intersect(incoming)
  .map:
    case (Accept, ranges) => ranges.combinations
    case (Reject, ranges) => 0L
    case (Send(to), ranges) => search(to, ranges)
  .sum

val ans2 = search("in", Ranges.full)
