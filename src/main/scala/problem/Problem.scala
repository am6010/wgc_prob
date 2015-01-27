package problem

/**
 * Created by Alexandros Milaios on 11/10/14 at 10:41 PM.
 *
 */
class Problem {


  sealed abstract class Position {
    def change: Position
  }
  case object East extends Position {
    def change: Position = West
  }
  case object West extends Position {
    def change: Position = East
  }

  sealed abstract class Move
  case object Nothing extends Move
  case object Wolf extends Move
  case object Goat extends Move
  case object Cabbage extends Move


  case class State(m: Position, w: Position, g: Position, c: Position) {
    def nothing = State(m.change, w, g, c)
    def goat = State(m.change, w, g.change, c)
    def wolf = State(m.change, w.change, g, c)
    def cabbage = State(m.change, w, g, c.change)

    def getFollowingStates: List[(State, Move)] =
      (nothing, Nothing) ::(goat, Goat) ::(wolf, Wolf) ::(cabbage, Cabbage) :: Nil

    def validFollowingStates: List[(State, Move)] = getFollowingStates filter(_._1.isLegal)

    def isLegal: Boolean = (m == w && m == g) || (m == g && g == c) || (g != w && g !=c)
  }

  val startState: State = new State(East, East, East, East)

  val goalState: State = new State(West, West, West, West)

  def nextStateWitheHistory(state: State, history: List[Move]) : Stream[(State,List[Move])] = {
    state.validFollowingStates map ({case (s, m) => (s, m :: history)}) toStream
  }

  def newStatesOnly(states: Stream[(State, List[Move])], explored: Set[State]): Stream[(State, List[Move])] = {
    states filter ({case(s,_) => ! (explored contains s)})
  }

  def from(initial: Stream[(State, List[Move])], explored: Set[State]) : Stream[(State, List[Move])] = {
    if (initial.isEmpty) Stream.empty
    else {
      val (s, h) = initial.head
      val next = newStatesOnly(nextStateWitheHistory(s,h),explored)
      (s,h) #:: from(initial.tail ++ next, explored ++ (next map (_._1)))
    }
  }

  lazy val pathsFromStart = from(Stream((startState, Nil)), Set(startState))

  lazy val pathsToGoal = pathsFromStart.filter({case(s, _) => s == goalState})

  lazy val solution = if(pathsToGoal.isEmpty) Nil
    else {
    val (_, h) = pathsToGoal.head
    h.reverse
  }
}
