package object arrays {


  def singleNumber(nums: Array[Int]): Int = {
    nums.foldLeft[Set[Int]](Set.empty)
      {(agg,n) => if(agg.contains(n)) agg - n else agg + n}.head
  }


  singleNumber(Array(2,3,3,4,4))
  singleNumber(Array.emptyIntArray)


  def singleNumber2(nums: Array[Int]): Int = {
    2 * nums.distinct.reduce(_ + _) - nums.reduce(_ + _)


  }

  def singleNumber3(nums: Array[Int]): Int = nums.reduce(_ ^ _)



  def removeDuplicates(nums: Array[Int]): Int =
    if(nums.length < 2) nums.length else (1 until nums.length).foldLeft(0){case(i,j) => if(nums(i) < nums(j)) {
      nums(i+1) = nums(j)
      i+1
    } else i } + 1

  def strStr(haystack: String, needle: String): Int = {
    def loop(input: String, index: Int): Int =
      if(input.take(needle.length) == needle) index
      else if(index > haystack.length) -1
      else loop(input.drop(1), index+1)

    loop(haystack,0)
  }

}
