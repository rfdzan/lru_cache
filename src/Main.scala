import scala.collection.mutable.Map
class lruCache[T]:
  def has(key: String): Boolean =
    ???
  def get(key: String): T =
    ???
  def set(key: String, value: T) =
    ???

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
  def popFront: T | Unit =
    val toReturn = firstNode match
      case None       => return
      case Some(node) => node.content
    val removedNode = firstNode
    firstNode = removedNode match
      case None       => return
      case Some(node) => node.nextNode
    return toReturn

  def popBack: T | Unit =
    val toReturn = lastNode match
      case None       => return
      case Some(node) => node.content
    val removedNode = lastNode
    lastNode = removedNode match
      case None       => return
      case Some(node) => node.prevNode
    return toReturn

@main def main =
  val dll = new doublyLinkedList[String]
  val myArr = Vector("once", "upon", "a", "time")
  myArr.map((str) => dll.pushToBack(str))
  myArr.foreach((_) => println(dll.popFront)) 
