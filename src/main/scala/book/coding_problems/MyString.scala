package com.endsoul.fp.scala
package book.coding_problems

import scala.jdk.StreamConverters._
import scala.util.Try

object MyString {
  def countDuplicateCharacters(str: String): Map[String, Long] = {
    val codePoints: LazyList[Int] = str.codePoints.toScala(LazyList)
    codePoints
      .map(c => Try(Character.toChars(c)).toOption)
      .map(c => c.map(a => String.valueOf(a)))
      .flatten
      .foldLeft(Map.empty[String, Long])((m, str) => {
        m.get(str) match {
          case Some(v) => m + (str -> (v + 1))
          case None    => m + (str -> 1)
        }
      })
  }
}
