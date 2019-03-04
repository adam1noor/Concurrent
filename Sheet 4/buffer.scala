import io.threadcso._
class BoundedBuffer[T] (max : Int) {
    private val mutex = MutexSemaphore()
    private val canWrite = SignallingSemaphore()
    private val canRead = SignallingSemaphore()
    private val buffer = scala.collection.Queue[T]()
    private val length = 0

    canWrite.up
    mutex.up

    def enqueue (input : T) = {
        canWrite.down
        mutex.down
        buffer.enqueue
        length +=1
        if (length < max) canWrite.up
        mutex.up
        canRead.up
    }
    def dequeue : T = {
        canRead.down
        mutex.down
        buffer.dequeue
        length -=1
        if (length == 0) canRead.down
        mutex.up
        canWrite.up
    }
}