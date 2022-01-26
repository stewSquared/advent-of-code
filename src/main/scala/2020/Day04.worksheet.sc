import util.Using

val passports = Using(io.Source.fromResource("2020/day-04.txt")){ source =>
  val lines = source.getLines
  val ps = new collection.mutable.ListBuffer[String]()
  while lines.hasNext
  do ps += lines.takeWhile(_.nonEmpty).mkString(" ")
  ps.result()
}.get

passports foreach println

passports(0)
passports(1)
passports(2)

val fields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

val ans1 = passports.count(p => fields.forall(p.contains))

def valid(passport: String): Boolean =
  val kvs = passport.split(" ")
  kvs.map { case s"$key:$value" => key -> value }
    .forall {
      case ("byr", digits) => 1920 to 2002 contains digits.toInt
      case ("iyr", digits) => 2010 to 2020 contains digits.toInt
      case ("eyr", digits) => 2020 to 2030 contains digits.toInt
      case ("hgt", s"${cm}cm") => 150 to 193 contains cm.toInt
      case ("hgt", s"${in}in") => 59 to 76 contains in.toInt
      case ("hcl", s"#$hex") => hex.sizeIs == 6 && hex.forall(c => ('0' to '9' contains c) || ('a' to 'f' contains c))
      case ("ecl", ecl) => "amb blu brn gry grn hzl oth".split(" ").contains(ecl)
      case ("pid", number) => number.sizeIs == 9
      case ("cid", _) => true
      case _ => false
    }


valid("iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

valid("hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277")


// val s"#$hex" = "hcl:dab227"

// val hex = "123abz"
// hex.forall(c => ('0' to '9' contains c) || ('a' to 'f' contains c))

// val ecl = "brn"
// "amb blu brn gry grn hzl oth".split(" ").contains(ecl)

// val s"${in}in" = "190in"
// 59 to 76 contains in.toInt
// val s"${cm}cm" = "190cm"
// 150 to 193 contains cm.toInt


val ans2 = passports
  .filter(p => fields.forall(p.contains))
  .count(valid)

  // kvs.map { case s"$key:$value" => key -> value }.toMap
  // kvs.get("byr").collect { case digits if }
