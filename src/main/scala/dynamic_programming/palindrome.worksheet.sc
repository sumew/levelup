/**
  * Th
  *
  *
  *
  *
  */
def longestPalindrome(input: String): String = {
  val cache = scala.collection.mutable.Map.empty[(Int, Int), Boolean]
  var max = (0, 0)

  def loop(left: Int, right: Int): Boolean = cache.get(left, right) match {
    case Some(b) => b
    case None =>
      (right - left) match {
        case n if n <= 0 => true
        case n => {
          println(s"left: $left, right: $right, max: $max")
          for (r <- left to right)
            for (l <- left to r) {
              val p = input.charAt(l).equals(input.charAt(r)) && loop(l + 1, r - 1)
              cache.update((l, r), p)
              if (p && (max._2 - max._1) < r - l)
                max = (l, r)
            }
          cache.getOrElse(max, false)
        }
      }
  }
  if (input.isEmpty())
    ""
  else {
    loop(0, input.length - 1)
    input.substring(max._1, max._2 + 1)
  }

}

longestPalindrome("babad")

longestPalindrome("cbbd")

longestPalindrome("")
longestPalindrome("bb")
