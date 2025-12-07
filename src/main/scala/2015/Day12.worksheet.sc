val input = io.Source.fromResource("2015/day-12.txt").getLines().next()

import util.chaining.*

val numbers = "-?[0-9]+".r
val ans1 = numbers.findAllMatchIn(input).map(_.toString.toInt).sum

enum Json:
  case JStr(value: String)
  case JNum(value: Int)
  case JArr(values: Vector[Json])
  case JObj(values: Map[JStr, Json])

import Json.*

case class Parser(input: String, index: Int):
  def current = input.charAt(index)
  def skipWhitespace: Parser = ???
  def advance(n: Int = 1): Parser = copy(index = index + n)
  def expect(char: Char): Parser =
    if current == char then advance(1) else
      throw new Exception(s"expected $char at $index. found $current")

  def parseString: (JStr, Parser) =
    val end = input.indexWhere(_ == '"', index + 1)
    val v = input.slice(index, end)
    val state = this.advance(end - index).expect('"')
    println(s"parsed $v at $index - ${state.index}")
    (JStr(v), state)

  def parseNumber: (JNum, Parser) =
    val end = input.indexWhere(!_.isDigit, index + 1).pipe:
      i => if i == -1 then input.length else i
    val v = input.slice(index, end).toInt
    val state = this.advance(end - index)
    println(s"parsed $v at $index - ${state.index}")
    (JNum(v), state)

  def parseArray: (JArr, Parser) =
    if current == ']' then (JArr(Vector()), this.expect(']')) else
      def parse(state: Parser, acc: Vector[Json]): (JArr, Parser) =
        if state.current == ']' then (JArr(acc), state.expect(']'))
        else
          val (v, s) = state.expect(',').parseValue
          parse(s, acc.appended(v))
      val (first, state) = this.parseValue
      parse(state, Vector(first))

  def parseKV: ((JStr, Json), Parser) =
    val (key, state1) = this.advance(1).parseString
    val (value, state2) = state1.expect(':').parseValue
    println(s"parsed $key -> $value, at $index, moved to ${state2.index}")
    (key -> value) -> state2

  def parseObject: (JObj, Parser) =
    if current == '}' then (JObj(Map()), this.expect('}')) else
      def parse(state: Parser, acc: Map[JStr, Json]): (JObj, Parser) =
        if state.current == '}' then (JObj(acc), state.expect('}'))
        else
          val ((k, v), s) = state.expect(',').parseKV
          parse(s, acc.updated(k, v))
      val (first, state) = this.parseKV
      parse(state, Map(first))

  def parseValue: (Json, Parser) = input(index) match
    case '"' => this.advance(1).parseString
    case '[' => this.advance(1).parseArray
    case '{' => this.advance(1).parseObject
    case _ => this.parseNumber

Parser("1234-----", 0).parseNumber
Parser("1234\"-----", 0).parseString
Parser("12,34,\"aa\"]", 0).parseArray
Parser(""""red":33,"green":"four","blue":0}""", 0).parseObject
  ._1

val (json, _) = Parser(input, 0).parseValue

def filterRed(json: Json): Json = json match
  case v@JObj(values) => JObj.apply:
    values.filter:
      case (k, v: JObj) => v.values.values.forall(_ != JStr("red"))
      case _ => true
    .view.mapValues(filterRed).toMap
  case v@JArr(values) => JArr.apply:
    values.filter:
      case (v: JObj) => v.values.values.forall(_ != JStr("red"))
      case _ => true
    .map(filterRed)
  case v => v

def sum(json: Json): Int = json match
  case JStr(value) => 0
  case JNum(value) => value
  case JArr(values) => values.map(sum).sum
  case JObj(values) => values.values.map(sum).sum

sum(json) == ans1
ans1

val ans2 = sum(filterRed(json))

val j = Parser("""[1,{"c":"red","b":2},3]""", 0).parseValue._1
           show(j)
show(filterRed(j))

def show(js: Json): String = js match
  case JStr(value) => s"\"$value\""
  case JNum(value) => value.toString
  case JArr(values) => values.map(show).mkString("[",",","]")
  case JObj(values) => values.map:
      case (k, v) => s"${show(k)}:${show(v)}"
    .mkString("{",",","}")
