package week02

case class Pouring(capacity: Vector[Int]) {

  // States
  type State = Vector[Int]
  val initialState = capacity map (_ => 0)

  // Moves
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val glasses = capacity.indices

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  // Paths
  class Path(history: List[Move]) {
    def endState: State = (history foldRight initialState) (_ change _)
    def extend(move: Move) = new Path(move :: history)
    override def toString = history.reverse mkString " " + "--> " + endState
  }

  val initialPath = new Path(Nil)

  def from(paths: Set[Path]): Stream[Set[Path]] = ???
}
