package week03.discrete_event_sim

abstract class Simulation {

  type Action = () => Unit
  private type Agenda = List[Event]
  case class Event(time: Int, action: Action)

  private var agenda: Agenda = List()
  private var curtime = 0
  def currentTime: Int = curtime

  private def insert(ag: Agenda, item: Event): Agenda = ag match {
    case first :: rest if first.time <= item.time =>
      first :: insert(rest, item)
    case _ =>
      item :: ag
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
      println("*** simulation ended, time = " + currentTime + " ***")
  }
  def run(): Unit = {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    loop()
  }
}
