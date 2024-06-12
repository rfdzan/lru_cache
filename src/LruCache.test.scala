//> using toolkit default
import LinkedList.doublyLinkedList
import LruCache.lruCache
class LruTests extends munit.FunSuite:
  test("has: return true for existing key and false otherwise") {
    val newLru = lruCache[Int](5)
    assert(newLru.has("one") == false)
    newLru.set("one", 1)
    assert(newLru.has("one"))
  }
  test("has: return false for evicted keys") {
    val newLru = lruCache[Int](1)
    newLru.set("one", 1)
    newLru.set("two", 2)
    assert(newLru.has("one") == false)
    assert(newLru.has("two") == true)
  }
  test("has: returns true for multiple keys") {
    val newLru = lruCache[Int](5)
    newLru.set("one", 1)
    newLru.set("two", 2)
    assert(newLru.has("one"))
    assert(newLru.has("two"))
  }
  test("get: return none for non-existent key otherwise returns its value") {
    val newLru = lruCache[Int](5)
    newLru.set("one", 1)
    assert(newLru.get("one") == Some(1))
    assert(newLru.get("two") == None)
  }
  test("get: return none for evicted key, Some(value) otherwise") {
    val newLru = lruCache[Int](1)
    newLru.set("one", 1)
    newLru.set("two", 2)
    assert(newLru.get("one") == None)
    assert(newLru.get("two") == Some(2))
  }

  test("get: return value for multiple existing keys") {
    val newLru = lruCache[Int](5)
    newLru.set("one", 1)
    newLru.set("two", 2)
    assert(newLru.get("one") == Some(1))
    assert(newLru.get("two") == Some(2))
  }
  test("set: should set record") {
    val newLru = lruCache[Int](5)
    newLru.set("one", 1)
    newLru.set("two", 2)
    assert(newLru.get("one") == Some(1))
    assert(newLru.get("two") == Some(2))
  }
  test("set: overwrites previous record with the same key") {
    val newLru = lruCache[Int](5)
    newLru.set("one", 1)
    newLru.set("one", 5)
    assert(newLru.get("one") == Some(5))
  }
  test("set: sets record while evicting") {
    val newLru = lruCache[Int](2)
    newLru.set("one", 1)
    newLru.set("two", 2)
    newLru.set("three", 3)
    assert(newLru.get("one") == None)
    assert(newLru.get("two") == Some(2))
    assert(newLru.get("three") == Some(3))
  }
  test("takes 'get' operation into account when evicting") {
    val newLru = lruCache[Int](3)
    newLru.set("one", 1)
    newLru.set("two", 2)
    newLru.set("three", 3)
    assert(newLru.get("one") == Some(1))
    newLru.set("four", 4)
    assert(newLru.get("two") == None)
    assert(newLru.get("three") == Some(3))
    assert(newLru.get("four") == Some(4))
  }
  test("takes 'has' operation into account when evicting") {
    val newLru = lruCache[Int](3)
    newLru.set("one", 1)
    newLru.set("two", 2)
    newLru.set("three", 3)
    assert(newLru.has("one") == true)
    newLru.set("four", 4)
    assert(newLru.get("two") == None)
    assert(newLru.get("three") == Some(3))
    assert(newLru.get("four") == Some(4))

  }
