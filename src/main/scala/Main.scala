import javax.swing.tree.TreeNode
import scala.:+
import scala.collection.mutable.ArrayBuffer

object Driver extends App {
  //1
  def isAnagram(s: String, t: String): Boolean = {
    val first = s.toCharArray.sortWith(_<_)
    val second = t.toCharArray.sortWith(_<_)
    first.sameElements(second)
  }

  //isAnagram("anagram", "nagaram")

  //2
  def canMakeArithmeticProgression(arr: Array[Int]): Boolean = {
    val sortetArr = arr.sortWith(_>_)
    val result = (for(i <- 0 until sortetArr.length - 1) yield sortetArr(i) - sortetArr(i+1)).toSet
    result.size == 1
  }

  //println(canMakeArithmeticProgression(Array(5,1,3)))

  //3
  def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val ans = nums1.intersect(nums2)
    ans.distinct
  }

  //println(intersection(Array(4,9,5), Array(9,4,9,8,4)).mkString("Array(", ", ", ")"))
  //println(intersection(Array(1, 2, 2, 1), Array(2, 2)).mkString("Array(", ", ", ")"))

  //4
  def canFormArray(arr: Array[Int], pieces: Array[Array[Int]]): Boolean = {
    val arrayBuf = Array.emptyIntArray
    val buffer = arrayBuf.toBuffer
    for (i <- arr) {
      for (p <- pieces) {
        if (p.contains(i)) {
          for (pi <- p) {
            buffer.addOne(pi)
          }
        }
      }
    }
    arr.sameElements(buffer.distinct)
  }

  //println(canFormArray(Array(91,4,64,78), Array(Array(78), Array(4,64), Array(91))))

  //5
  def largestPerimeter(nums: Array[Int]): Int = {
    val sorted = nums.sortWith(_>_)
    for(i <- 0 until sorted.length - 2) {
      if (sorted(i) + sorted(i+1)> sorted(i+2) && sorted(i) + sorted(i+2)> sorted(i+1) && sorted(i+2) + sorted(i+1)> sorted(i)) {
        return sorted(i) + sorted(i+1) + sorted(i+2)
      }
    }
    return 0
  }

  //println(largestPerimeter(Array(3,2,3,4)))
  //println(largestPerimeter(Array(3,6,2,3)))


  //6
    class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
     var value: Int = _value
     var left: TreeNode = _left
     var right: TreeNode = _right
    }

  def maxDepth(root: TreeNode): Int = {
    if(root == null) return 0
    val maxi = scala.math.max(maxDepth(root.left), maxDepth(root.right))
    maxi +1
  }

  //7
  class Node(var _value: Int) {
      var value: Int = _value
      var children: List[Node] = List()
    }
  def maxDepth(root: Node): Int = {
    if(root == null) return 0
    if (root.children == List.empty) 1
     1 + root.children.map(child => maxDepth(child)).max
  }

  def maximumUnits(boxTypes: Array[Array[Int]], truckSize: Int): Int = {
    var result = 0
    var curr = 0
    while (curr < truckSize) {
       for (i <- boxTypes) {
        result += i(1) * i(2)
        curr += i(1)
      }
    }
    result
  }
  println(maximumUnits(Array(Array(1,3), Array(2,2), Array(3,1)), 4))
}
