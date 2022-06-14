import foam._

case class CacheState(val id: Int) extends State {
  override val isAccept = true
}

object CacheStateFactory {
  private var stateCount = 0;

  def apply() = {
      stateCount += 1
      CacheState(stateCount)
  }
}