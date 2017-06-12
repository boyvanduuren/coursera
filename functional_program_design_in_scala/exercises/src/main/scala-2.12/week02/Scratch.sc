def from(n: Int): Stream[Int] = n #:: from(n+1)

def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

(sieve(from(2)) take 100).toList

from(1) map (x => x%2 == 0)
