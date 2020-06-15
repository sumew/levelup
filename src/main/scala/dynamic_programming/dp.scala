object dp {

  trait DP[S, T, K, V] {

    def breakDown(state: S, choice: T): S
    def update(value: V, current: T): V
    def key(state: S): K

    def better(current: V, ultima: V): V
    def errorCheck(value: V): V

    val errorCase: S => Boolean

    val baseCase: S => Boolean
    val baseValue: V
    val errorValue: V
    val ultima: V

    def run(state: S, choices: Seq[T]): V = {

      val cache = scala.collection.mutable.Map.empty[K, V]

      def loop(s: S): V = cache.get(key(s)) match {

        case Some(v) => v
        case None =>
          s match {
            case b if baseCase(b)  => baseValue
            case e if errorCase(e) => errorValue
            case x => {
              var ult = ultima
              for (choice <- choices) {
                val m = loop(breakDown(s, choice))
                ult = better(update(m,choice), ult)
              }
              ult = errorCheck(ult)
              cache.update(key(x), ult)
              ult
            }
          }
      }

      loop(state)

    }

  }

}
