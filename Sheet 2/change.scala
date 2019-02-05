import io.threadcso._
def change (inPound: ?[Unit], out5p: ![Unit], out10p: ![Unit], out20p: ![Unit]) : PROC = proc {
    var credit = 0
    serve {
         (credit ==0 && inPound) =?=> {Unit => credit += 100}
        |(credit >= 5 && out5p)  =!=> {credit -= 5;  ()}
        |(credit >=10 && out10p) =!=> {credit -= 10; ()}
        |(credit >=20 && out20p) =!=> {credit -= 20; ()}
    }
}
/*
It's fair to the input ports as it uses serve. It can be
blocked if one of the output ports don't accept though.
*/