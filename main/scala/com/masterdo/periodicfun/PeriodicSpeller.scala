package com.masterdo.periodicfun

/**
  Interview question about recursivity, collections and strings.
  "Write a method that takes a string as input and tries to spell out that input string
  using only substrings present in the Periodic Table of Elements."
  */
class PeriodicSpeller(table: Map[String,Int]) {

  def spell(input: String): Option[Seq[Int]] = {

    val deadPaths = scala.collection.mutable.Set[String]()

    def validateSubstring(sub: String): Option[Seq[Int]] = {

      def spellSubstring(i: Int): Option[Seq[Int]] = {

        val lhs = sub.take(i)
        if (table.contains(lhs)){
          val index = table.get(lhs).get
          val rhs = sub.drop(i)
          if (rhs.length > 0) {
            validateSubstring(rhs) match {
              case Some(result) => return Some(Seq(index) ++ result)
              case None =>
            }
          } else {
            return Some(Seq(index))
          }
        }
        None
      }

      if (deadPaths.contains(sub)) {
        return None
      } else {
        for (i <- 1 to (if (sub.length >= 3) 3 else sub.length)) {
          spellSubstring(i) match {
            case Some(result) => return Some(result)
            case None =>
          }
        }
      }
      deadPaths += sub
      None
    }
    validateSubstring(input.toUpperCase);
  }
}
object PeriodicSpeller {

  def main(args: Array[String]): Unit = {
    val speller = new PeriodicSpeller(table)
    println(speller.spell(args(0)).getOrElse("No results."))
  }

  val table = Map(
    "B" -> 5,
    "AC" -> 89,
    "O" -> 8,
    "N" -> 7
    // Etc...
  )
}
