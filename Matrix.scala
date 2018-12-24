import io.threadcso._
type Matrix[T] = Array[Array[T]]

abstract class Matrix_MultT(a: Matrix[Double], b:Matrix[Double]){
	//Calculate the Matrix
	def multiply(): Matrix[Double]
}

class Matrix_Mult(a: Matrix[Double], b:Matrix[Double], 
				nWorkers : Int) extends Matrix_MultT(a,b){

	//The resultant Matrix
	val c : Matrix[Double] = Array.ofDim[Double](a.length,b(0).length)
	
	//Task contains a row and coloumn to be multiplied
	private type Task = (Int, Int)

	//Send a task to the workers
	private val toWorkers = OneMany[Task]

	//Return the position and the value
	private val toCollector = ManyOne[(Task,Double)]

	//Checks if a Matrix is rectangular
	private def checkShape(mat : Matrix[Double])= proc{
		var row_size = mat(0).length
		for (i<- 0 until mat.length) {
			if (mat(i).length != row_size) {
				throw new IllegalArgumentException(mat + " is not rectangular")
			}
		} 
	}

	//This Multiplies the Rows of a with the Coloumns of b (after they've been transposed)
	//First it checks that the row can coloumn can be multiplied
	private def worker() = proc("Worker"){
		repeat{
			val (row,col) = (toWorkers?)
			var res : Double = 0
			for (i <- 0 until a(0).length) {
				res += a(row)(i)*b(i)(col)
			}
			toCollector!((row,col),res)
		}
	}

	//This distributes a row and a colom pair to the workers
	private def distributor =proc("distributor") {
		for (i <- 0 until c(0).length) {
		 	for (j <- 0 until c.length){
		 		toWorkers!(i,j)
		 	}
		}
		toWorkers.close
	}
	
	private def collector = proc("collector") {
		for (i <- 1 to c(0).length*c.length) {
			val coordres = (toCollector?)
			c(coordres._1._1)(coordres._1._2) = coordres._2
		}
	}

	//This controls the overall proccess
	private def system() = proc("System"){
		if (a.length != b(0).length) {
			throw new IllegalArgumentException("Cannot be multiplied") 
		}
		run (checkShape(a) || checkShape(b))
		val workers = || (for (i<-0 until nWorkers) yield worker)
		run(workers || distributor || collector)
	}

	def multiply():Matrix[Double]={
		run(system); c
	}
}