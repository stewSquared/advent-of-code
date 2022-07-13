val input = io.Source.fromResource("2020/day-19.txt").getLines.toList

enum Rule:
  case MatchString(s: String)
  case AndThen(a: Rule, b: Rule)
  case Either(a: Rule, b: Rule)
  case Ref(id: Int)

import Rule.*

object Rule:
  def parse(raw: String): Rule = raw match
    case s""""$s"""" => MatchString(s)
    case s"$a | $b" => Either(parse(a), parse(b))
    case s"$a $b" => AndThen(parse(a), parse(b))
    case ref => Ref(ref.toInt)

val rules: Map[Int, Rule] = input.collect {
  case "8: 42" => 8 -> Rule.parse("42 | 42 8")
  case "11: 42 31" => 11 -> Rule.parse("42 31 | 42 11 31")
  case s"$id: $rawRule" => id.toInt -> Rule.parse(rawRule)
}.toMap

extension (rule: Rule)
  def matchStart(message: String): Vector[String] = rule match
    case MatchString(s) => Option.when(message.startsWith(s))(message.stripPrefix(s)).toVector
    case AndThen(a, b) => a.matchStart(message).flatMap(b.matchStart)
    case Either(a, b) => a.matchStart(message) ++ b.matchStart(message)
    case Ref(id) => rules(id).matchStart(message)

  def valid(message: String): Boolean =
    rule.matchStart(message).contains("")

val messages = input.dropWhile(_.contains(":")).tail

val ans = messages.count(rules(0).valid)
