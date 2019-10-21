import scala.annotation.tailrec

object WordFunnel extends App {
  import scala.io.Source
  val html = Source.fromURL("https://raw.githubusercontent.com/dolph/dictionary/master/enable1.txt")
  val words = html.mkString.split('\n').toSet

  val word = "gnash"

  implicit val funnelOrdering: Ordering[Set[String]] = (x, y) => x.size - y.size

  def isWord: String => Boolean = words.contains

  def minLetter(w: String): Set[String] = {
    @tailrec
    def iterate(i: Int, left: String, right: String): String = {
      if(right.isEmpty) left
      else if(i == 0) left + right.tail
      else iterate(i - 1, left + right.head, right.tail)
    }

    (0 until w.length).map(i => iterate(i, "", w)).toSet
  }

  println(minLetter("abro")) // Set(bro, aro, abo, abr)

  // Longest funnel length
  def funnel0(w: String, acc: Int = 0): Int =
    if (words.contains(w)) minLetter(w).map(ww => funnel0(ww, acc + 1)).max
    else acc

  println(funnel0(word)) // 4

  // Longest funnel
  def funnel1(w: String, acc: Set[String] = Set()): Set[String] =
    if (words.contains(w)) minLetter(w).map(ww => funnel1(ww, acc + w)).max
    else acc

  println(funnel1(word)) // Set(gnash, gash, ash, sh)


  // All funnels
  def funnel2(w: String, acc: Set[String] = Set()): Set[Set[String]] =
    if(words.contains(w)) minLetter(w).flatMap(ww => funnel2(ww, acc + w))
    else Set(acc)

  println(funnel2(word).toList.sorted.reverse) // List(Set(gnash, gash, ash, ah), Set(gnash, gash, ash, sh), Set(gnash, gash, gas, as), Set(gnash, gash, ash, as), Set(gnash, gash, gas), Set(gnash, gash), Set(gnash))

  println(funnel2("princesses")) // HashSet(HashSet(pi, princess, princesse, princes, princesses, pis, prices, pies, pries), ...



  // Optional 2 WIP
  @tailrec
  def isSubwordOf(s: String, w: String): Boolean = {
    if(w.isEmpty) s.isEmpty
    else if(s.isEmpty) true
    else if(w.length < s.length) false
    else if(s.head == w.head) isSubwordOf(s.tail, w.tail)
    else isSubwordOf(s, w.tail)
  }

  println(isSubwordOf("aba", "abbadon")) // true

}
