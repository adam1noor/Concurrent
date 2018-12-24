import io.threadcso._

object Sorter{
	// This Provides a channel for the results to be fed back
	val toOut = ManyOne[Double]

	def main(args: Array[String]) = {
		//Stores the numbers input
		val list = args.map(_.toDouble)
		val n = list.length
		//Produces Channels for the comparators to use
		val channels =new Array[Chan[Double]](n+1)
		for (i<- 0 to n) {
			channels(i) = OneOne[Double]
		}
		//This produces the comparators
		val comparators = || (for (i<- 0 until n) yield comparator(channels(i),channels(i+1)))
		run(inout || comparators || cleaner(n))

		//This handles the actually running of the program
		def inout= proc("inout"){
			//Inputting the numbers in turn
			for (i<- 0 until n) {
				channels(0)!(list(i))
			}
			//Signals input is finished
			channels(0).close
			//Generates a Result Array
			val res = new Array[Double](n)
			for (i<- 0 until n) {
				res(i)=toOut?
			}
			println(res.deep)
		}
		//Takes the waste output on the last channel
		//(n negative infinities)
		def cleaner(size: Int)= proc{
			repeat(true){channels(n)?}
		}
	}

		// To make finding min and max easeir
	def maxmin(x1: Double, x2: Double) :(Double,Double) = {
		if (x1<x2) {return (x2,x1)}
		else { return (x1,x2)}
	}

	//This compares 2 values, keep the larger and passes on the smaller
	// It has 2 channels for passing values.
	def comparator(left: Chan[Double], right: Chan[Double]) = proc("comparator"){
		//Inital value
		var value = Double.NegativeInfinity
		//The current pair being stored
		var pair = (value,value)
		var input : Double = 0
		repeat(true) {
			input = (left?)		
			pair = maxmin(input,value)
			//Store the larger one
			value = pair._1
			//Pass on the Smaller one.
			right!(pair._2)
		}
		//Report the value back and pass signal on
		toOut!(value)
		right.close
	}


}
