import scala.util.Random

trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}

// random integer generator
val integers = new Generator[Int] {
  override def generate: Int = Random.nextInt
}

// random boolean generator, we can use for because `integers` implements trait Generator
val booleans = for (x <- integers) yield x > 0

// choose a value between lo and hi
def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- integers) yield lo + Math.abs(x) % (hi - lo)

// choose one of a collection of objects
def oneOf[T](xs: T*): Generator[T] = for (idx <- choose(0, xs.length)) yield xs(idx)

// a generator that generates the same element every time
def single[T](x: T): Generator[T] = new Generator[T] {
  override def generate: T = x
}

// an empty list generator
def emptyLists = single(Nil)

// a non empty list generator
def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail

// generate a random list
def lists: Generator[List[Int]] = for {
  isEmpty <- booleans
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list
