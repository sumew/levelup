package object sliding_window {

  object SlidingWindow {
    case class Window(left: Int, right: Int){
      def open() = Window(left, right+1)
      def close() = Window(left+1, right)
      def width() = right  - left
    }

    case class State[S](current: Window, best: Window, value: S){
      def withCurrent(_current: Window): State[S] = this.copy(current = _current)
      def withBest(_best: Window): State[S] = this.copy(best = _best)
      def withValue(_value: S): State[S] = this.copy(value = _value)
    }
  }

  trait SlidingWindow[R,S,T] {
    import SlidingWindow._
    val input: Seq[T]
    val pattern: R
    val initialState: State[S]

    def satisfies(state: State[S]): Boolean
    def add(t: T, state: State[S]): State[S]
    def remove(t: T, state: State[S]): State[S]
    def updateBest(state: State[S]): State[S]


    def expand(state: State[S]): State[S] =
      if(satisfies(state)) shrink(state)
      else if(state.current.right >= input.length) state
      else expand(add(input(state.current.right), state))

    def shrink(state: State[S]): State[S] =
      if(!satisfies(state)) expand(state)
      else shrink(remove(input(state.current.left), updateBest(state)))

  }




}
