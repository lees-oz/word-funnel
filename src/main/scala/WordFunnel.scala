object WordFunnel extends App {
  import scala.io.Source
  val html = Source.fromURL("https://raw.githubusercontent.com/dolph/dictionary/master/enable1.txt")
  val words = html.mkString.split('\n').toSet

  val word = "gnash"

  implicit val funnelOrdering: Ordering[Set[String]] = (x, y) => x.size - y.size

  def isWord: String => Boolean = words.contains

  def minusLetter(w: String): Set[String] =
    (0 until w.length)
      .map(i =>
        new StringBuilder(w)
          .deleteCharAt(i)
          .toString())
    .toSet

  def funnel0(w: String, acc: Int): Int =
    if (words.contains(w)) minusLetter(w).map(ww => funnel0(ww, acc + 1)).max
    else acc

  println(funnel0(word, 0)) // 4


  def funnel1(w: String, acc: Set[String]): Set[String] =
    if (words.contains(w)) minusLetter(w).map(ww => funnel1(ww, acc + w)).max
    else acc

  println(funnel1(word, Set())) // Set(gnash, gash, ash, sh)


  def funnel2(w: String, acc: Set[String]): Set[Set[String]] =
    if(words.contains(w)) minusLetter(w).flatMap(ww => funnel2(ww, acc + w))
    else Set(acc)

  println(funnel2(word, Set()).toList.sorted.reverse) // List(Set(gnash, gash, ash, ah), Set(gnash, gash, ash, sh), Set(gnash, gash, gas, as), Set(gnash, gash, ash, as), Set(gnash, gash, gas), Set(gnash, gash), Set(gnash))
}
