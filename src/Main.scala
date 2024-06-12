package Main
import scala.collection.mutable.Map

case class MapValue[T](
    val value: T,
    var node: Node[T]
):
  def getVal: T =
    return value

class lruCache[T](itemLimit: Int):
  private var map: Map[String, MapValue[T]] = Map()
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
  private def mostRecentlyUsed(key: String, mapVal: MapValue[T]) =
    // TODO: move cache hit keys to the end of the list
    val storedNodeNextNode = mapVal.node.nextNode
    val storedNodePrevNode = mapVal.node.prevNode
    mapVal.node.nextNode match
      case None => None
      case Some(node) =>
        node.prevNode = storedNodePrevNode
    mapVal.node.prevNode match
      case None => None
      case Some(node) =>
        node.nextNode = storedNodeNextNode
    map.update(key, mapVal)
  def readQueue =
    dll.readAll
  def has(key: String): Boolean =
    map.get(key) match
      case None        => false
      case Some(value) =>
        // this is probably not passing by reference
        mostRecentlyUsed(key, value)
        // dll.pushToBack(Node(key))
        true
  def get(key: String): Option[T] =
    map.get(key) match
      case None =>
        // cache miss
        return None
      case Some(value) =>
        // cache hit
        mostRecentlyUsed(key, value)
        // dll.pushToBack(Node(key))
        return Some(value.getVal)
  def set(key: String, value: T): Unit =
    evict(key)
    dll.pushToBack(Node(key))
    map.addOne(key, MapValue(value, Node(value)))

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
  val newLru = lruCache[Int](3)
  newLru.set("one", 1)
  newLru.set("two", 2)
  newLru.set("three", 3)

  newLru.readQueue
  println()
  newLru.has("two")
  newLru.readQueue
  // assert(newLru.get("two") == None)
  // assert(newLru.get("three") == Some(3))
  // assert(newLru.get("four") == Some(4))
