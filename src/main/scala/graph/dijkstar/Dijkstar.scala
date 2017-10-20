package graph.dijkstar

import graph.model.ColorEnum.WHITE
import graph.model.{Edge, Graph, Node}

import scala.collection.mutable

class Dijkstar {

  def findShortest(graph: Graph, s: Node, e: Node) = {

    s.distance = 0

    implicit val order: Ordering[Node] = Ordering.by[Node, Long](n => n.distance).reverse

    var queue = new mutable.PriorityQueue[Node]()

    for (n <- graph.nodes) {
      if (n != s) {
        n.distance = Long.MaxValue
        n.previousNode = None
      }
    }

    queue.enqueue(graph.nodes: _*)

    while (queue.nonEmpty) {
      val n = queue.dequeue()
      for (e <- n.edges) {
        relax(e, n)
      }

      val tmp = queue.dequeueAll
      queue = new mutable.PriorityQueue[Node]()
      queue.enqueue(tmp: _*)
    }

    var path = List.empty[String]
    var tmp = Option(e)
    while (tmp.get.name != s.name) {
      path = tmp.get.name :: path
      tmp = tmp.get.previousNode
    }

    (e.distance, (s.name :: path).mkString(" --> "))
  }

  private def relax(edge: Edge, n: Node): Unit = {
    val alt = n.distance + edge.weight
    if (alt < edge.end.distance) {
      edge.end.distance = alt
      edge.end.previousNode = Some(n)
    }
  }

}

object Dijkstar {
  def main(args: Array[String]): Unit = {

  }
}
