import io.threadcso._
def multiplex[T](in0: ?[T], in1: ?[T], out0: ![T], ou1: ![T]) {

    val mid = OneOne[(Int,T)]

    def tagger[T] = proc {
        serve{
             in0 =?=> {x => mid!(1,x) }
            |in1 =?=> {x => mid!(0,x) }
        }
        in0.close; in1.close; mid.close
    }

    def detagger[T] = proc{
        repeat {
            val (channel,x) = mid?()
            if (channel == 0) out0!x else out1!x
        }
        out0.close; ou1.close; mid.close
    }
    tagger || detagger
}

/*
It's fair to the input ports as it uses serve. It can be
blocked if one of the output ports don't accept though.
*/