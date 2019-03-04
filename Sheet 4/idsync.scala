import io.threadcso._
class IDLockSemaphore{
  /** Number of readers in the CS. */
  private var sumID = 0

  /** Semaphore to protect shared variables. */
  private val mutex = MutexSemaphore()

  /** Semaphore to signal that something can enter. */
  private val canEnter = SignallingSemaphore()
  canEnter.up
  
  def enter(id: Int) = {
    canEnter.down
    mutex.down
    sumID += id
    if (sumID % 3 == 0) canEnter.up
    mutex.up
  }

  def exit (id:Int) = {
    mutex.down
    sumID -= id
    if (sumID %3 == 0) canEnter.up
    else mutex.up
  }
}