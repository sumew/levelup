object SlidingWindow {

  case class Window(left: Int, right: Int) {
    def open() = Window(left, right + 1)

    def close() = Window(left + 1, right)

    def width() = right - left
  }

  case class State[S](current: Window, best: Window, value: S) {
    def withCurrent(_current: Window): State[S] = this.copy(current = _current)

    def withBest(_best: Window): State[S] = this.copy(best = _best)

    def withValue(_value: S): State[S] = this.copy(value = _value)
  }

}

trait SlidingWindow[R, S, T] {

  import SlidingWindow._

  val input: Seq[T]
  val pattern: R
  val initialState: State[S]

  def satisfies(state: State[S]): Boolean

  def add(t: T, state: State[S]): State[S]

  def remove(t: T, state: State[S]): State[S]

  def updateBest(state: State[S]): State[S]


  def expand(state: State[S]): State[S] = {
    println(s"expand($state)")

    if (satisfies(state)) shrink(state)
    else if (state.current.right >= input.length) state
    else expand(add(input(state.current.right), state))
  }

  def shrink(state: State[S]): State[S] = {
    println(s"shrink($state)")
    if (!satisfies(state)) expand(state)
    else shrink(remove(input(state.current.left), updateBest(state)))
  }

}

import SlidingWindow._

class SubArraySum(override val input: Seq[Int], override val pattern: Int) extends SlidingWindow[Int, Int, Int] {
  override def add(_value: Int, state: State[Int]): State[Int] =
    state.withValue(_value + state.value)
      .withCurrent(state.current.open())

  override def remove(_value: Int, state: State[Int]): State[Int] =
    state.withValue(state.value - _value)
      .withCurrent(_current = state.current.close())

  override val initialState = State(Window(0, 0), Window(0, 0), 0)

  override def satisfies(state: State[Int]) = state.value >= pattern

  override def updateBest(state: State[Int]) =
    state.withBest(
      if (state.best.width() > state.current.width() || state.best.width() == 0) state.current
      else state.best
    )
}


def minSubArrayLen(s: Int, nums: Array[Int]): Int = {

  val sas = new SubArraySum(nums, s)
  val state = sas.expand(sas.initialState)
    println(s"final state: $state")
      state.best.width()
}


minSubArrayLen(7, Array(2, 3, 1, 2, 4, 3))
