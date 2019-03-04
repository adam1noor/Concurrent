import io.threadcso._

trait ManWomanSync{
    def manSync(me : String) : String
    def womanSync (me : String) : String
}

class ManWomanMonitor extends ManWomanSync {
    private var men = 0; private var women = 0;
    private var manName : String = ""
    private var womanName : String =""
    //Invariant: men <= 1 && women <= 1
    def manSync(me:String) : String = synchronized{
        men +=1
        manName = me
        notifyAll()
        while (women == 0) wait()
        men -=1
        return womanName
    }
    def womanSync(me:String) : String = synchronized{
        women +=1
        womanName = me
        notifyAll()
        while (men == 0) wait()
        women -=1
        return manName
        //This waits until there is 1 man and she has changed the manName
    }
}

class ManWomanSemaphores extends ManWomanSync {
    private var manName : String =""
    private var womanName : String = ""
    private var mentex = MutexSemaphore()
    private var womentex = MutexSemaphore()
    private var manhere = SignallingSemaphore()
    private var womanhere = SignallingSemaphore()
    def manSync(me:String) : String = {
        mentex.down
        manName = me
        manhere.up
        womanhere.down
        var pair = womanName
        mentex.up
        return pair
    }
    def womanSync(me:String) : String = {
        womentex.down
        womanName = me
        womanhere.up
        manhere.down
        var pair = manName
        womentex.up
        return pair
    }
}