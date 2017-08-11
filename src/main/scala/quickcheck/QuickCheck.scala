package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      k <- arbitrary[A]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  /** HINTS: *************************************************************************
    * If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
    * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    * ********************************************************************************
    */


  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("Find the min of 2 elements should return the smallest element") = forAll { (element1: A, element2: A) =>
    def element = insert(element2, empty)

    def elements = insert(element1, element)

    def heaps = elements

    findMin(heaps) == min(element1, element2)
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("Insert and delete the minimal value in a Heap results in an empty Heap") = forAll { (element: A) =>
    def addToEmptyHeap = insert(element, empty)

    def emptyHeap = deleteMin(addToEmptyHeap)

    isEmpty(emptyHeap)
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("Continually find and delete elements in a Heap to return a sorted sequence") = forAll { (heap: H) =>
    def loop(heap: H): Boolean = {
      if (isEmpty(heap)) true
      else {
        val minElement = findMin(heap)
        val newHeap = deleteMin(heap)
        isEmpty(newHeap) || (minElement <= findMin(newHeap) && loop(newHeap))
      }
    }

    // returns true when the heap is sorted
    loop(heap)
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("The minimal of the melding of any two Heaps should return the min of one or the other heap") = forAll { (heap1: H, heap2: H) =>
    findMin(meld(heap1, heap2)) == min(findMin(heap1), findMin(heap2))
  }


  // comparing if two heaps are equal if their sorted elements are equal
  property("if two heaps min elements removed are equal in sequence, then two heaps are equal.") = forAll { (heap1: H, heap2: H) =>
    def loop(heap1: H, heap2: H): Boolean =
      if (isEmpty(heap1) && isEmpty(heap2)) true
      else {
        val minElement1 = findMin(heap1)
        val newHeap1 = deleteMin(heap1)
        val minElement2 = findMin(heap2)
        val newHeap2 = deleteMin(heap2)
        minElement1 == minElement2 && loop(newHeap1, newHeap2)
      }

    // returns true when the heaps are sorted
    def meldedHeaps = meld(heap1, heap2)
    loop(meldedHeaps, meld(deleteMin(heap1), insert(findMin(heap1), heap2)))
  }

}
