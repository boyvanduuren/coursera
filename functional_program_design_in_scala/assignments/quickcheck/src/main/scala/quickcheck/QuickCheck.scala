package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
      i <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("smallest") = forAll { (h: H) =>
    val min = if (isEmpty(h)) 0 else findMin(h)
    val evenless = min - 1
    findMin(insert(evenless, h)) == evenless
  }

  property("empty after add + delete") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("contains sorted") = forAll { (h: H) =>
    def recurseTillEmpty(heap: H, previous: A): Boolean = {
      if (isEmpty(heap)) true
      else {
        val min = findMin(heap)
        previous < min && recurseTillEmpty(deleteMin(heap), min)
      }
    }

    recurseTillEmpty(deleteMin(h), findMin(h))
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val h1Min = findMin(h1)
    val h2Min = findMin(h2)

    val meldMin = findMin(meld(h1, h2))
    meldMin == h1Min || meldMin == h2Min
  }

}
