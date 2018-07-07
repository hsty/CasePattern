object temp extends App{

  abstract class IntTree
  case object EmptyTree extends IntTree
  case class Node(elem: Int, left: IntTree, right: IntTree) extends IntTree

  def contains(t: IntTree, v: Int): Boolean = t match {
    case EmptyTree => false
    case Node(e, l, r) => {
      if(e == v) true
      else if (v<e) contains(l, v)
      else contains(r,v)
    }
  }

  def insert(t: IntTree, v: Int): IntTree = t match {
    case EmptyTree => new Node(v, EmptyTree, EmptyTree)
    case Node(e, l, r) => {
      if(e==v) t
      else if(v<e) new Node(e, insert(l,v), r)
      else new Node(e, l, insert(r,v))
    }
  }

  val two = new Node(2, EmptyTree, EmptyTree)
  val four = new Node(4, EmptyTree, EmptyTree)
  val six = new Node(6, EmptyTree, EmptyTree)
  val three = new Node(3, two, four)
  val five = new Node(5, three, six)

  println(contains(five, 7))
  println(contains(five, 2))
  println(insert(five, 1))
  println(insert(five, 2))


  }
