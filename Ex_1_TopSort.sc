import scala.annotation.tailrec
import scala.collection.mutable

object session {

  class Node[IdT, DataT >: Null](val id: IdT, val data: DataT = null) {
    override def toString: String = s"Node($id, $data)"
  }

  class Graph[T >: Null] {

    var idCounter: Int = 0
    val nodes = new mutable.HashMap[Int, Node[Int, T]]()
    val adjacent = new mutable.HashMap[Int, mutable.HashSet[Int]]()
    val edges = new mutable.HashSet[(Int, Int)]()

    def createNode(data: T = null): Node[Int, T] = {
      val node = new Node[Int, T](idCounter, data)
      nodes(idCounter) = node
      adjacent(idCounter) = new mutable.HashSet[Int]()
      idCounter += 1
      node
    }

    def createNode(): Node[Int, T] = createNode(null)

    def addEdge(node1: Node[Int, T], node2: Node[Int, T]): Unit = {
      if (!nodes.contains(node1.id))
        throw new IllegalArgumentException(s"Nodes $node1 is not belong this graph.")
      if (!nodes.contains(node2.id))
        throw new IllegalArgumentException(s"Nodes $node2 is not belong this graph.")
      adjacent(node1.id).add(node2.id)
      edges.add((node1.id, node2.id))
    }

    def dfs(node: Node[Int, T], func: Node[Int, T] => Unit): Unit = {
      val visited = new mutable.HashSet[Int]()
      dfsHelper(node, visited, func)
    }

    def dfsHelper(node: Node[Int, T], visited: mutable.HashSet[Int], func: Node[Int, T] => Unit): Unit = {
      func(node)
      visited.add(node.id)
      for (adjId <- adjacent(node.id)) {
        if (!visited.contains(adjId)) {
          val adjNode = nodes(adjId)
          dfsHelper(adjNode, visited, func)
        }
      }
    }

  }

  class TopologicalGraph[T >: Null] extends Graph[T] {

    def topoDfsHelper(node: Node[Int, T], visited: mutable.HashSet[Int], ordered: mutable.ArrayBuffer[Int]): Unit = {
      if (!visited.contains(node.id)) {
        visited += node.id
        for (adjNodeId <- adjacent(node.id)) {
          val adjNode = nodes(adjNodeId)
          topoDfsHelper(adjNode, visited, ordered)
        }
        ordered += node.id
      }
    }

    def allNodesTopological(func: Node[Int, T] => Unit): Unit = {
      val visited = new mutable.HashSet[Int]()
      val ordered = new mutable.ArrayBuffer[Int]()
      val origins = new mutable.HashSet[Int]()
      for (node <- nodes.valuesIterator)
        origins.add(node.id)
      for ((node1, node2) <- edges.iterator) {
        if (origins.contains(node2))
          origins.remove(node2)
      }
      println(origins)
      val superOrigin = createNode()
      for (nodeId <- origins) {
        val node = nodes(nodeId)
        addEdge(superOrigin, node)
      }
      topoDfsHelper(superOrigin, visited, ordered)
      for (nodeId <- ordered.reverseIterator) {
        val node = nodes(nodeId)
        func(node)
      }
    }
  }


  val g = new TopologicalGraph[String]
  val a = g.createNode("a")
  val b = g.createNode("b")
  val c = g.createNode("c")
  val d = g.createNode("d")
  val e = g.createNode("e")

  g.addEdge(a, c)
  g.addEdge(a, b)
  g.addEdge(a, d)
  g.addEdge(b, d)
  g.addEdge(c, d)
  g.addEdge(a, e)
  g.addEdge(c, e)
  g.addEdge(d, e)

  println("DFS")
  g.dfs(a, (node: Node[Int, String]) => {println(node.data)})

  println("Topological Sorting")
  g.allNodesTopological((node: Node[Int, String]) => { println(node) })
}