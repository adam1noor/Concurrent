import scala.language.postfixOps
import io.threadcso._
import scala.collection.mutable.Queue

object bufferTest{
    def buff[T](in : ?[T], out: ![T]) = proc {
    var buffer = new Queue[T]
    print("Started")
    serve (
         in =?=> {y => buffer.enqueue(y);println("Inputted")}
        |out =!=> { buffer.dequeue}
        )
    }

  def main(args: Array[String]) = {
    val intArr = args.map(x => x.toInt)
    val in = OneOne[Int]
    val out = OneOne[Int]
    fork(buff(in,out))
    for (i <- 0 until intArr.length) {
        in!(intArr(i))
    }
    for (i<-0 until intArr.length) {
        println(out?)
    }
    in.close
    out.close
  }
}
/* Is this it?*/