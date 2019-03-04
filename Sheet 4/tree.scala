import io.threadcso._
import scala.language.postfixOps

abstract class IntTree
case class Leaf(n: Int) extends IntTree
case class Branch(l:IntTree, r:IntTree) extends IntTree


def rec_sum (tree: IntTree, toParent: ![Int]) : PROC = proc{
    tree match {
        case Leaf(n) => toParent!(n)
        case Branch(l,r) =>
            {
             var lChannel = OneOne[Int]
             var rChannel = OneOne[Int]
             var sum = 0
             rec_sum(l, lChannel).fork
             rec_sum(r, rChannel).fork
             sum += lChannel?()
             sum += rChannel?()
             toParent!(sum) }
    }
}

def sum(tree : IntTree) : PROC = proc{
    var child = OneOne[Int]
    rec_sum(tree, child).fork
    var total = child?()
    print(total)
}

var tree : IntTree = Branch(Branch(Leaf(12),Leaf(3)),Leaf(4))
/* Works!!!!!!!!! :)
 * Have to do "run(sum(tree)) in the interpreter"
 */