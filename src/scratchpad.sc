




strStr("miiba","ba")


def twoSum(nums: Array[Int], target: Int): Array[Int] = {
  val initial = (0 until nums.length -1 ).find(idx => nums(idx) + nums(idx+1) == target).get
  Array(initial, initial+1)
}
