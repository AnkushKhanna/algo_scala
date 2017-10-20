package graph.model

import graph.model.ColorEnum.Color

class Node(val name: String, var color: Color) {

  var edges: List[Edge] = List.empty

  var previousNode: Option[Node] = None

  var distance = Long.MaxValue

  def addEdges(weight: Long, end: Node) = {
    edges = Edge(weight, this, end) :: edges
  }

  def getAdj(): List[Edge] = {
    edges
  }

  override def toString = s"Node: ($name)"
}

object Node {
  def apply(name: String, color: Color) = new Node(name, color)
}

case class Edge(weight: Long, start: Node, end: Node)

class Graph {
  var nodes: List[Node] = List.empty[Node]

  def addNode(node: Node) = {
    nodes = node +: nodes
  }

  def addEdge(weight: Long, start: Node, end: Node) = start.addEdges(weight, end)

  def getAdj(node: Node) = node.edges
}

object ColorEnum {

  sealed trait Color

  case object WHITE extends Color

  case object GREY extends Color

  case object BLACK extends Color

}
