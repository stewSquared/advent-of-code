val input = "bgvyzdsv"

// calculate md5 hash using standard library
import java.security.MessageDigest
def md5(s: String): String =
  val digest = MessageDigest.getInstance("MD5").digest(s.getBytes)
  digest.map("%02x".format(_)).mkString

md5("abcdef" + 609043.toString)

val ans1 = Iterator.from(1).find: n =>
  md5(input + n.toString).take(5).forall(_ == '0')
.get

// val ans2 = Iterator.from(1).find: n =>
//   md5(input + n.toString).take(6).forall(_ == '0')
// .get
