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
case class Send(to: String) extends Action

sealed trait Condition:
  def applicable(part: Part): Boolean =
    this match
      case LT(category, value) => part.category(category) < value
      case GT(category, value) => part.category(category) > value

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

val workFlows = Map.from[String, List[Rule]]:
  val lines = input.takeWhile(_.nonEmpty)
  lines.map[(String, List[Rule])]:
    case s"$name{$rawRules}" =>
      val rules = rawRules.split(",").toList.map:
        case s"$condition:$action" =>
          Conditional(Condition.fromString(condition), Action.fromString(action))
        case action => Action.fromString(action)
      name -> rules.toList

val parts: List[Part] = input.map(Part.fromString).toList

def applyWorkflow(part: Part, workflow: List[Rule]): Action =
  val action = workflow.collectFirst:
    case Conditional(condition, action) if condition.applicable(part) => action
    case action: Action => action
  action.get

def process(part: Part) =
  LazyList.unfold[Action, Action](Send("in")):
    case Send(to) =>
      val action = applyWorkflow(part, workFlows(to))
      Some(action -> action)
    case Accept => None
    case Reject => None

def accepted(part: Part) = process(part).contains(Accept)

val ans1 = parts.filter(accepted).map(_.ratingNumber).sum

parts.foreach: part =>
  println(process(part).mkString(" -> "))





//
