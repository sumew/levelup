

def minSubArrayLen(s: Int, nums: Array[Int]): Int = {

  case class State(left: Int, right: Int, sum: Int, current: (Int,Int))

  def expand(state: State): State = {
    println(s"expand called with state: $state")
    if(state.right >= nums.length) state
    else {

      if(state.sum >= s){
        shrink(state.copy(
          current =
            if(state.right - state.left < state.current._2 - state.current._1 || state.sum == 0)
              (state.left,state.right)
            else state.current
        )

        )

      }
      else
      expand(state.copy(sum = state.sum + nums(state.right), right = state.right + 1))
    }
  }

  def shrink(state: State): State = {
    println(s"shrink called with state: $state")
    if(state.sum < s) expand(state)
    else {
      shrink(state.copy(left = state.left+1,
        sum = state.sum - nums(state.left),
      current =
        if(state.sum - nums(state.left)>= s && state.right - state.left < state.current._2 - state.current._1 || state.sum == 0) (state.left + 1, state.right)
        else state.current
      ))
    }
  }

  val (left,right) = expand(State(0,0,0,(0,0))).current
  println(s"left: $left, right: $right")
  (right - left)


}

val nums = Array(1,4,4)
val s = 4

minSubArrayLen(s,nums)