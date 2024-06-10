//> using toolkit default
import Main.lruCache
class LruTests extends munit.FunSuite:
  test("eviction") {
    val newLru = lruCache[Int](5)  
    assert(newLru.has("one") == false)
    newLru.set("one", 1)
    newLru.set("two", 2)
    newLru.set("three", 3)
    newLru.set("four", 4)
    newLru.set("five", 5)
    assert(newLru.has("one"))
    newLru.set("six", 6)
    assert(newLru.has("one") == false)
    assert(newLru.has("six"))
  }
  test("get values") {
    val newLru = lruCache[Int](5)  
    newLru.set("one", 1)
    newLru.set("two", 2)
    newLru.set("three", 3)
    newLru.set("four", 4)
    newLru.set("five", 5)
    assert(newLru.get("one") == Some(1))
    assert(newLru.get("five") == Some(5))
  }
  test("same key overwrites old values") {
    val newLru = lruCache[Int](5)  
    newLru.set("one", 1)
    newLru.set("one", 2)
    newLru.set("one", 3)
    newLru.set("one", 4)
    newLru.set("one", 5)
    assert(newLru.get("one") == Some(5))
  }
