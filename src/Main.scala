package Main
import scala.collection.mutable.Map

case class MapValue[T](
    private val value: T,
    private val node: Node[T]
):
  def getVal: T =
    return value
  def getNode: Node[T] =
    return node

class lruCache[T](itemLimit: Int):
  private var map: Map[String, MapValue[T]] = Map()
  private var dll = new doublyLinkedList[String]
  private def evict(key: String, put: Boolean): Unit =
    val plusOne = if put then 1 else 0
    if map.size + plusOne > itemLimit then
      // get key from ddl
      // use popped key from ddl to
      // remove map entry possesing that key.
      val popped = dll.popFront match
        case None      => None
        case Some(key) => 
          map.remove(key)
          dll.pushToBack(key)
    // TODO: move cache hit keys to the end of the list
  def has(key: String): Boolean =
    map.get(key) match
      case None        => false
      case Some(value) => 
        evict(key, false)
        true
  def get(key: String): Option[T] =
    val mapVal = map.get(key) match
      case None => 
        // cache miss
        None
      case Some(value) =>
        // cache hit
        evict(key, false)
        Some(value.getVal)
    return mapVal
  def set(key: String, value: T): Unit =
    evict(key, true)
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
  def pushToFront(data: T): Unit =
    val newNode = Some(Node(data))
    if firstNode == None then
      firstNode = newNode
      lastNode = newNode
      return
    newNode match
      case Some(node) => node.addNextNode(firstNode)
    firstNode match
      case None       => return
      case Some(node) => node.addPreviousNode(newNode)
    firstNode = newNode
  def pushToBack(data: T): Unit =
    val newNode = Some(Node(data))
    if firstNode == None then
      firstNode = newNode
      lastNode = newNode
      return
    newNode match
      case Some(node) => node.addPreviousNode(lastNode)
    lastNode match
      case None       => return
      case Some(node) => node.addNextNode(newNode)
    lastNode = newNode
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
  println()
