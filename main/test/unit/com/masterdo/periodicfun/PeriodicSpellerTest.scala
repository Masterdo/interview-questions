package com.masterdo.periodicfun

import org.scalatest.FlatSpec

class PeriodicSpellerTest extends FlatSpec{

  "A speller" should "find matches requiring no backtracking" in {
    val table = Map(
      "A" -> 1,
      "B" -> 2,
      "C" -> 3
    )

    val speller = new PeriodicSpeller(table)

    val expectedResult = Seq(1,2,3)
    val actualResult = speller.spell("abc").get

    assert(actualResult equals expectedResult)
  }

  it should "find matches requiring backtracking" in {
    val table = Map(
      "A" -> 1,
      "AB" -> 2,
      "ABC" -> 3
    )

    val speller = new PeriodicSpeller(table)

    val expectedResult = Seq(3)
    val actualResult = speller.spell("abc").get

    assert(actualResult equals expectedResult)
  }

  it should "return None when no matches exist" in {
    val table = Map(
      "A" -> 1,
      "B" -> 2,
      "D" -> 3
    )

    val speller = new PeriodicSpeller(table)

    val expectedResult = None
    val actualResult = speller.spell("abc")

    assert(actualResult equals expectedResult)
  }

  // Test to illustrate an edge case that would take much longer without the map of failed paths
  it should "use the deadPaths set to prevent futile recursion" in {
    val table = Map(
      "P" -> 1,
      "O" -> 2,
      "PO" -> 3
    )

    val speller = new PeriodicSpeller(table)

    val expectedResult = None
    val actualResult = speller.spell("popopopopopopopopopopopopoxxxx")

    assert(actualResult equals expectedResult)
  }
}
