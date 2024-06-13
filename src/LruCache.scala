package LruCache
import scala.collection.mutable.Map
import LinkedList.{Node, doublyLinkedList}
case class MapValue[T, U](
    val value: T,
    var node: Node[U]
):
  def getVal: T =
    return value

class lruCache[T](itemLimit: Int):
  private val map: Map[String, MapValue[T, String]] = Map()
  private val dll = new doublyLinkedList[String]
  private def evict(key: String): Boolean =
    if map.size + 1 > itemLimit then
      // use popped key from dll to
      // remove map entry possesing that key.
      dll.popFront match
        case None => return false
        case Some(key) =>
          map.remove(key)
          return true
    return false
  private def mostRecentlyUsed(key: String, mapVal: MapValue[T, String]): Unit =
    // if the most recently used key is already at the tail, don't touch it.
    if dll.lastNode == Some(mapVal.node) then return
    val storedNodeNextNode = mapVal.node.nextNode
    val storedNodePrevNode = mapVal.node.prevNode
    mapVal.node.nextNode match
      case None       => None
      case Some(node) =>
        // if the original mapVal.node is the first node of the list,
        // the pattern matched mapVal.node.nextNode becomes the first node.
        //
        // [node1, node2, node3, node4, node5]
        //    ^og    ^
        // [node2, node3, node4, node5, node1]
        //    ^                           ^og
        //
        // original mapVal.node moves to the tail, as it is the most recently used.
        if Some(mapVal.node) == dll.firstNode then
          node.prevNode = None
          dll.firstNode = Some(node)
        else node.prevNode = storedNodePrevNode
    mapVal.node.prevNode match
      case None       => None
      case Some(node) =>
        // earlier checks prior to this block and the pattern matching towards 'Some' case makes sure that when entering this block,
        // the original mapVal.node must have come from somewhere in the middle, it will never be the first or the last node.
        node.nextNode = storedNodeNextNode
    dll.pushToBack(Node(key)) match
      case None       =>
      case Some(node) =>
        // updates the map with the node returned from a call to pushToBack,
        // new node is returned from the call to pushToBack which sets its previous and next node element accordingly.
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
        // new node is returned from the call to pushToBack which sets its previous and next node element accordingly
        map.addOne(key, MapValue(value, node))
