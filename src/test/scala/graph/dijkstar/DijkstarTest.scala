package graph.dijkstar

import graph.model.ColorEnum.WHITE
import graph.model.{Graph, Node}
import org.scalatest.FlatSpec

class DijkstarTest extends FlatSpec {

  val g = new Graph()
  val a = Node("a", WHITE)
  val b = Node("b", WHITE)
  val c = Node("c", WHITE)
  val d = Node("d", WHITE)
  val e = Node("e", WHITE)

  g.addNode(a)
  g.addNode(b)
  g.addNode(c)
  g.addNode(d)
  g.addNode(e)

  g.addEdge(10, a, b)
  g.addEdge(5, a, c)
  g.addEdge(2, b, c)
  g.addEdge(1, b, d)
  g.addEdge(3, c, b)
  g.addEdge(2, c, e)
  g.addEdge(9, c, d)
  g.addEdge(4, d, e)
  g.addEdge(6, e, d)
  g.addEdge(7, e, a)

  "Dijkstar" should "return correct values a --> d" in {

    val (distance, path) = new Dijkstar().findShortest(g, a, d)
    assert(9 === distance)
    assert("a --> c --> b --> d" === path)
  }

  it should "return correct values a --> e" in {

    val (distance, path) = new Dijkstar().findShortest(g, a, e)
    assert(7 === distance)
    assert("a --> c --> e" === path)
  }

}
