package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("min1") = forAll { a:Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back
  property("findMin over a 2-element heap should return the min of the two values") = forAll {  (a: Int, b:Int) =>
      val h = insert(a, insert(b, empty))
      findMin(h) == math.min(a, b)      
  }
  
  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("empty heap after inserting an element in a empty heap and then deleting it") = forAll {
    a: Int =>
      val h = insert(a, empty)
      deleteMin(h) == empty
  }
  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("Getting a sorted sequence from a random heap") = forAll { h: H =>
    def getSortedHeap(h:H) : List[Int] = h match {
      case h if h==empty => Nil
      case h => findMin(h) :: getSortedHeap(deleteMin(h))
    }
    
    val listOfSortedHeap = getSortedHeap(h)
    (listOfSortedHeap, listOfSortedHeap.tail).zipped.forall(_ <= _)
  }
  
  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("finding minimum of melding of two heaps") = forAll {
    (h1:H, h2:H) =>
      findMin(meld(h1, h2)) == math.min(findMin(h1), findMin(h2))
  }
  
  // get min after inserting value > min in a heap should yield the true min
  property("get min after inserting value > min in a heap should yield min") = forAll {
    (a: Int) =>
    val b: Int = if(a == Int.MaxValue) a+1 else a
    val h1 = insert(b+1, insert(b, insert(b+2, empty)))
    
    deleteMin(h1) == insert(b+2, insert(b+1, empty))
  }

  // 2 insert and 1 deleteMin should not yield an empty heap
  property("2 insert and 1 deleteMin should not yield an empty heap") = forAll {
    (a: Int) =>
      val h1 = insert(a, insert(a, empty))
      val h2 = deleteMin(h1)
      !isEmpty(h2)
  }
}
