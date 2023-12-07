import math.Ordering.Implicits.given

val input = io.Source.fromResource("2023/day-07.txt").getLines().toList

enum Card:
  case Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two

given Ordering[Card] = Ordering.by(-_.ordinal)

enum Hand:
  case FiveKind, FourKind, FullHouse, ThreeKind, TwoPair, OnePair, HighCard

given Ordering[Hand] = Ordering.by(-_.ordinal)

import Hand.*
import Card.*

def parseCard(c: Char) = c match
  case '2' => Two
  case '3' => Three
  case '4' => Four
  case '5' => Five
  case '6' => Six
  case '7' => Seven
  case '8' => Eight
  case '9' => Nine
  case 'T' => Ten
  case 'J' => Jack
  case 'Q' => Queen
  case 'K' => King
  case 'A' => Ace


val hands: List[(Hand, Vector[Card], Int)] = input.map: line =>
  val s"$cardsStr $bid" = line
  val cards = cardsStr.map(parseCard).toVector
  (chooseHand(cards), cards, bid.toInt)

def chooseHand(cards: Vector[Card]): Hand =
  val cardGroups = cards.groupMapReduce(identity)(_ => 1)(_ + _).toList.sortBy((c, count) => count -> c).reverse
  cardGroups match
    case List((card, 5)) => FiveKind
    case List((card, 4), (card2, 1)) => FourKind
    case List((card, 3), (card2, 2)) => FullHouse
    case List((card, 3), (card2, 1), (card3, 1)) => ThreeKind
    case List((card, 2), (card2, 2), (card3, 1)) => TwoPair
    case List((card, 2), (card2, 1), (card3, 1), (card4, 1)) => OnePair
    case highCard => Hand.HighCard

val sorted = hands.sortBy:
  case (hand, cards, bid) => (hand, cards)

val scores = sorted.zipWithIndex.map:
  case ((_, _, bid), i) => bid * (i + 1)

val ans1 = scores.sum
