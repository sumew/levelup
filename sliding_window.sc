import scala.annotation.tailrec

/**
  * Given a string S and a string T, find the minimum window in S which will contain all the characters in T in complexity O(n).
  * Example:
  * Input: S = "ADOBECODEBANC", T = "ABC"
  * Output: "BANC"
  * Note:
  * If there is no such window in S that covers all characters in T, return the empty string "".
  * If there is such window, you are guaranteed that there will always be only one unique minimum window in S.
  **/

def minWindow(s: String, t: String): String = {

  case class State(left: Int, right: Int, rem: Map[Char, Int], current: (Int, Int))

  def updateCurrent(left: Int, right: Int, count: Int, current: (Int, Int)): (Int, Int) =
    if (count == 0) current
    else if ((current._2 - current._1) == 0 || (right - left) < (current._2 - current._1)) (left, right)
    else current

  @tailrec
  def shrink(state: State): State = {
    //println(s"shrink called with state: $state, current: ${s.substring(state.current._1, state.current._2)}, window: ${s.substring(state.left, state.right)}")
    state.rem.values.find(_ > 0) match {
      case None =>  shrink(state.copy(left = state.left + 1,
        rem = state.rem.get(s.charAt(state.left)).fold(state.rem) { count => state.rem.updated(s.charAt(state.left), count + 1) },
        current = updateCurrent(state.left + 1, state.right, state.rem.get(s.charAt(state.left)).fold(-1)(identity), state.current)))
      case _ => expand(state)
    }
  }

  @tailrec
  def expand(state: State): State = {
    //println(s"expand called with state: $state, current: ${s.substring(state.current._1, state.current._2)}, window: ${s.substring(state.left, state.right)}")
    state.rem.values.find(_ > 0) match {//Does window contain t?
      case None => shrink(state.copy(current = updateCurrent(state.left, state.right, -1, state.current)))
      case _ => if (state.right >= s.length) state else state.rem.get(s.charAt(state.right)) match {
        case None => expand(state.copy(right = state.right + 1))
        case Some(count) => expand(state.copy(right = state.right + 1, rem = state.rem.updated(s.charAt(state.right), count - 1)))
      }
    }

  }

  val state = expand(State(0, 0, t.foldLeft(Map[Char, Int]()) { (acc, c) => acc.updated(c, acc.get(c).fold(1)(_ + 1)) }, (0, 0)))
  s.substring(state.current._1, state.current._2)
}


minWindow("ADOBECONDEBANC", "ABC")