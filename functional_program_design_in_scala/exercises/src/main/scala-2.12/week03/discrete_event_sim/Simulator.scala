package week03.discrete_event_sim

class Simulator {

  class Wire
  def inverter(input: Wire, output: Wire): Unit
  def andGate(a1: Wire, a2: Wire, output: Wire): Unit
  def orGate(o1: Wire, o2: Wire, output: Wire): Unit

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) = {
    val d = new Wire
    val e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }
}
