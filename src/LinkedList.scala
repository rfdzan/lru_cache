package LinkedList
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
