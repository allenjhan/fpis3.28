object TreeMapTest extends App{
  val myTree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))
  println(Tree.map(myTree)(x => x%2 == 1))
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

}