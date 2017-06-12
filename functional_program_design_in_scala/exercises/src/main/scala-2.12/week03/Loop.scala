package week03

object Loop {

  def repeat(f: Unit)(condition: => Boolean): Unit = {
    f
    if (condition) () else repeat(f)(condition)
  }
}
