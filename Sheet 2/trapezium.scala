import io.threadcso._
import scala.language.postfixOps

/** Abstract class, representing the problem of estimating the integral of f
  * from a to b. */
abstract class EstimateT(f: Double => Double, a: Double, b: Double){
  require(a <= b)

  /** Calculate the integral. */
  def apply(): Double

  /** Use trapezium to calculate integral of f from left to right,
    * using 1 interval of size l-r.*/
  protected def estimate(left: Double, right: Double) : Double = {
    var height = (f(right)+f(left))/2.0
    var width = right-left
    return (height*width)
  }
}

// ==================================================================

/** Class to calculated the integral of f from a to b, using nWorkers workers
  * processes, with an adpative number of intervals and tasks.  This version
  * encapsulates the concurrency within objects. */
class EstimateBagObjects(f: Double => Double, a: Double, b: Double, nWorkers: Int)
    extends TrapeziumT(f, a, b){

  /** Epsilon is the margin of eror */
  private val epsilon = 1E-5

  /** Type of tasks to send to client.  The Task (left, right)
    * represents the task of calculating the integral from left to right*/
  private type Task = (Double, Double)

  /** The bag of tasks object. */
  private class BagOfTasks{

    // Estimates still to be allocated a worker.
    val pending =newscala.collection.mutable.Queue[Task]

    /** Channel from the controller to the workers, to distribute tasks. */
    private val toWorkers = OneMany[Task]

    /** Get a task.  
      * @throws Stopped exception if there are no more tasks. */
    def getTask: Task = toWorkers?

    /** Puts a task on the stack. */
    def putTask(task : Task) = toDistrib!task

    /** A server process, that distributes tasks. */
    private def server = proc{
      while (not(pending.isEmpty)){
        toWorkers!(pending.dequeue)
      }
      toWorkers.close
    }

    // Start the server running
    pending.enqueue((a,b))
    server.fork
  }

  /** A collector object that receives sub-results from the workers, and adds
    * them up. */
  private class Collector{
    /** Channel from the workers to the controller, to return sub-results. */
    private val toController = ManyOne[Double]

    /** Channel that sends the final result. */
    private val resultChan = OneOne[Double]

    /** A collector, that accumulates the sub-results. */
    private def server = proc{
      var result = 0.0
      for(i <- 0 until nTasks) result += (toController?)
      resultChan!result
    }

    // Start the server running
    server.fork

    /** Add x to the result. */
    def add(x: Double) = toController!x

    /** Get the result. */
    def get: Double = resultChan?
  }


  /** A worker, which repeatedly receives tasks from the BagOfTasks, estimates
    * the integral, and adds the result to the Collector. */
  private def worker(bag: BagOfTasks, collector: Collector) = proc{
    repeat{
      val (left, right) = bag.getTask
      val mid = (left+right)/2.0
      val result = estimate(left, right)
      val leftr = estimate(left,mid)
      val rightr = estimate (mid,right)
      if (Math.abs(leftr+rightr-result) < epsilon) collector.add(result)
      else {
        bag.putTask(left,mid)
        bag.putTask(mid,right)
      }
    }
  }
  /** Calculate the integral. */
  def apply: Double = {
    val bag = new BagOfTasks; val collector = new Collector
    val workers = || (for (i <- 0 until nWorkers) yield worker(bag, collector))
    workers()
    collector.get
  }
}
