package trees

import scala.collection.immutable.TreeMap
import scala.collection.mutable

class Tries[T] {

  class Node(val char: Option[Char], var value: Option[T] = Option.empty, var children: TreeMap[Char, Node] = TreeMap.empty)

  val root = new Node(char = Option.empty)

  def put(key: String, value: T): Unit = {
    put(key, value, root, 0)
  }

  def get(key: String): Option[T] = {
    get(key, root, 0)
  }

  def findKeysByPrefix(prefix: String): List[(String, T)] = {
    findNodeByPrefix(prefix) match {
      case None => List.empty
      case Some(n) =>
        val queue = new mutable.Queue[(String, T)]()
        findKeysByPrefix(n, new StringBuilder(prefix), queue)
        queue.toList
    }
  }

  def findShorterKeysByPrefix(prefix: String): List[(String, T)] = {
    findNodeByPrefix(prefix) match {
      case None => List.empty
      case Some(n) =>
        val queue = new mutable.Queue[(String, T)]()
        findShorterKeysByPrefix(n, new StringBuilder(prefix), queue)
        queue.toList
    }
  }

  private def findKeysByPrefix(node: Node, sb: StringBuilder, queue: mutable.Queue[(String, T)]): Unit = {
    if (node == null) return
    if (node.value.isDefined) {
      queue.enqueue((sb.toString(), node.value.get))
    }

    node.children.foreach(child => {
      sb.append(child._2.char.get)
      findKeysByPrefix(child._2, sb, queue)
      sb.deleteCharAt(sb.length - 1)
    })
  }

  private def findShorterKeysByPrefix(node: Node, sb: StringBuilder, queue: mutable.Queue[(String, T)]): Unit = {
    if (node == null) return
    if (node.value.isDefined) {
      queue.enqueue((sb.toString().trim(), node.value.get))
      return
    }

    node.children.foreach(child => {
      sb.append(child._2.char.get)
      findShorterKeysByPrefix(child._2, sb, queue)
      sb.deleteCharAt(sb.length - 1)
    })
  }

  private def findNodeByPrefix(prefix: String): Option[Node] = {
    findNodeByPrefix(prefix, root, 0)
  }

  private def findNodeByPrefix(prefix: String, node: Node, index: Int): Option[Node] = {
    if (prefix.length == index) {
      return Some(node)
    }

    node.children.get(prefix.charAt(index)) match {
      case None => None
      case Some(n) => findNodeByPrefix(prefix, n, index + 1)
    }

  }

  private def get(key: String, node: Node, index: Int): Option[T] = {
    if (key.length == index) {
      return node.value
    }

    node.children.get(key.charAt(index)) match {
      case None => None
      case Some(n) => get(key, n, index + 1)
    }
  }

  private def put(key: String, value: T, node: Node, index: Int): Node = {

    if (key.length == index) {
      node.value = Some(value)
      return node
    }

    val nextNode =
      node.children.get(key.charAt(index)) match {
        case None =>
          val n = new Node(Some(key.charAt(index)))
          node.children = node.children.insert(key.charAt(index), n)
          n
        case Some(n) =>
          n
      }

    put(key, value, nextNode, index + 1)
  }
}
