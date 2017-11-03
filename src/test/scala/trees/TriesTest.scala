package trees

import org.scalatest.FlatSpec

class TriesTest extends FlatSpec {

  "findKeysByPrefix" should "return correct key" in {
    val tries = new Tries[Int]
    tries.put("shop", 1)
    tries.put("show", 3)

    val values = tries.findKeysByPrefix("sh")
    assert(values(0)._1 === "shop")
    assert(values(0)._2 === 1)

    assert(values(1)._1 === "show")
    assert(values(1)._2 === 3)
  }

  "findShorterKeysByPrefix" should "return shortest key" in {
    val tries = new Tries[Int]
    tries.put("showdown", 1)
    tries.put("show", 3)

    val values = tries.findShorterKeysByPrefix("sh")

    assert(values.size === 1)
    assert(values(0)._1 === "show")
    assert(values(0)._2 === 3)
  }

  "get" should "return correct key" in {
    val tries = new Tries[Int]
    tries.put("showdown", 1)
    tries.put("show", 3)

    val v = tries.get("show")

    assert(v.isDefined)
    assert(v === Some(3))
  }


}
