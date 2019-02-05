import scala.language.postfixOps
import io.threadcso._
import scala.collection.mutable.Queue

trait PairServer{
  /** Client identities. */
  type Name = String

  /** Gender. true = female, false = male */
  type Gender = Boolean

  /** Request a resource. */
  def requestPair(me: Name, gender: Boolean): Name

  /** Shut down the server. */
  def shutdown
}

class pairServer1 extends PairServer{
    private val pairQ = Queue[(Name,Gender)]
    private val pairEnQ = ManyOne[(Name,Gender)]
    private val returnPair = OneMany[((Name,Gender),(Name,Gender))]

    //Return your pair given your name and gender
    def requestPair(me: Name, gender:Boolean) : Name = {
        pairEnQ!(me,gender)
        pairDeQ?(me,gender)
    }

    def server(): proc = PROC{
        serve(
            ()
        )
    }

}
/* I couldn't do this one :S*/