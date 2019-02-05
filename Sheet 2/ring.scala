 
import io.threadcso._
import scala.language.postfixOps

class Ring(f: T -> T -> T, n: Int) {
  require(n >= 2)

  /** Channels connecting the threads.  Channel chan(i) goes from thread (i-1)
    * mod n to thread i; so thread me inputs on chan(me) and outputs on
    * chan((me+1)%n). */
  private val chan = Array.fill(n)(OneOne[T])
 
  def apply(me: Int, x: T): T = {
    val in = chan(me); val out = chan((me+1)%n)
    if(me == 0){ // This is the initiator
      // Start the communications going
      out!(x)
      // Receive result back, and send them round
      val  result = in?()
      out! result
      // Receive them back at the end
      in?()
      result
    }

    else{
      // receive min and max so far, and pass on possibly updated min and max
      val xin = in?()
      out!(f(xin,x))
      // receive final min and max, and pass them on
      val res = in?()
      out!res
      res
    }
  } 
}

/*You need f to be associative and commutative for the symmetric ring to work
 *This is because otherwise you can end up with a different answer
 */