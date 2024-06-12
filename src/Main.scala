package Main
import scala.collection.mutable.Map

case class MapValue[T, U](
    val value: T,
    var node: Node[U]
):
  def getVal: T =
    return value

class lruCache[T](itemLimit: Int):
  private var map: Map[String, MapValue[T, String]] = Map()
  private var dll = new doublyLinkedList[String]
  private def evict(key: String): Boolean =
    if map.size + 1 > itemLimit then
      // get key from ddl
      // use popped key from ddl to
      // remove map entry possesing that key.
      dll.popFront match
        case None => return false
        case Some(key) =>
          map.remove(key)
          return true
    return false
  private def mostRecentlyUsed(key: String, mapVal: MapValue[T, String]): Unit =
    // if the most recently used value is already at the tail, don't touch it.
    if dll.lastNode == Some(mapVal.node) then return
    val storedNodeNextNode = mapVal.node.nextNode
    val storedNodePrevNode = mapVal.node.prevNode
    mapVal.node.nextNode match
      case None       => None
      case Some(node) =>
        // if the original node from mapVal is the first node of the list,
        // the pattern matched next node becomes the first node.
        //
        // [node1, node2, node3, node4, node5]
        //    ^og    ^
        // [node2, node3, node4, node5, node1]
        //    ^                           ^og
        //
        // original node moves to the tail, as it is the most recently used.
        if Some(mapVal.node) == dll.firstNode then
          node.prevNode = None
          dll.firstNode = Some(node)
        else node.prevNode = storedNodePrevNode
    mapVal.node.prevNode match
      case None       => None
      case Some(node) =>
        // if the original node from mapVal is the first node of the list,
        //
        node.nextNode =
          if dll.firstNode != Some(mapVal.node) then storedNodeNextNode else return
    dll.pushToBack(Node(key)) match
      case None       =>
      case Some(node) =>
        // updates the map with the node returned from a call to pushToBack,
        // which contains a node with the previous node and next node updated
        map.update(key, MapValue(mapVal.getVal, node))

  def readQueue =
    dll.readAll
  def has(key: String): Boolean =
    map.get(key) match
      case None => false
      case Some(value) =>
        mostRecentlyUsed(key, value)
        true
  def get(key: String): Option[T] =
    map.get(key) match
      case None =>
        // cache miss
        return None
      case Some(value) =>
        // cache hit
        mostRecentlyUsed(key, value)
        return Some(value.getVal)
  def set(key: String, value: T): Unit =
    dll.pushToBack(Node(key)) match
      case None       =>
      case Some(node) =>
        // checks if adding this new item would exceed the limit of the cache
        evict(key)
        // adds a new item to the cache
        // new node comes from the call to pushToBack which sets its previous and next node element accordingly
        map.addOne(key, MapValue(value, node))

class Node[T](data: T):
  var content: T = data
  var nextNode: Option[Node[T]] = None
  var prevNode: Option[Node[T]] = None
  def addNextNode(node: Option[Node[T]]) =
    nextNode = node
  def addPreviousNode(node: Option[Node[T]]) =
    prevNode = node

class doublyLinkedList[T](
    var firstNode: Option[Node[T]] = None,
    var lastNode: Option[Node[T]] = None
):
  def readAll: Unit =
    var currentNode = firstNode
    while (true)
      println(currentNode match
        case None        => return
        case Some(value) => Some(value.content)
      )
      currentNode match
        case None => return
        case Some(value) =>
          currentNode = value.nextNode
  def pushToFront(newNode: Node[T]): Option[Node[T]] =
    if firstNode == None then
      firstNode = Some(newNode)
      lastNode = Some(newNode)
      return Some(newNode)
    newNode.addNextNode(firstNode)
    firstNode match
      case None       => return None
      case Some(node) => node.addPreviousNode(Some(newNode))
    firstNode = Some(newNode)
    return Some(newNode)

  def pushToBack(newNode: Node[T]): Option[Node[T]] =
    if firstNode == None then
      firstNode = Some(newNode)
      lastNode = Some(newNode)
      return Some(newNode)
    newNode.addPreviousNode(lastNode)
    lastNode match
      case None       => return None
      case Some(node) => node.addNextNode(Some(newNode))
    lastNode = Some(newNode)
    return Some(newNode)

  def popFront: Option[T] =
    val toReturn = firstNode match
      case None       => None
      case Some(node) => Some(node.content)
    val removedNode = firstNode
    firstNode = removedNode match
      case None       => None
      case Some(node) => node.nextNode
    return toReturn

  def popBack: Option[T] =
    val toReturn = lastNode match
      case None       => None
      case Some(node) => Some(node.content)
    val removedNode = lastNode
    lastNode = removedNode match
      case None       => None
      case Some(node) => node.prevNode
    return toReturn

@main def main =
  val newLru = lruCache[Int](5)
  newLru.set("one", 1)
  newLru.set("two", 2)
  newLru.set("three", 3)
  newLru.has("one")
  newLru.set("four", 4)
  newLru.set("five", 5)
  newLru.readQueue
  println()
  newLru.has("one")
  newLru.readQueue
  println()
