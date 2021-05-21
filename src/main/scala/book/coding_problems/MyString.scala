package com.endsoul.fp.scala
package book.coding_problems

import scala.jdk.StreamConverters._
import scala.util.Try
import scala.collection.immutable.ListMap

import cats._
import cats.implicits._

// TODO: 자바 throws API 처리 Option or Try or Either
object MyString {
  // cover ASCII, 16-bit Unicode, and Unicode surrogate pairs
  def countDuplicateCharacters(str: String): Map[String, Long] =
    // 하나의 문자를 4bytes(Int) 배열로 변환
    str.codePoints
      .toScala(LazyList)
      .map(c => Try(Character.toChars(c)).toOption)
      .flatMap(_ map String.valueOf)
      .foldLeft(ListMap.empty[String, Long])((acc, str) => {
        acc.get(str) match {
          case Some(v) => acc + (str -> (v + 1))
          case None    => acc + (str -> 1)
        }
      })

  def firstNonRepeatedCharacter(str: String): String =
    countDuplicateCharacters(str)
      .dropWhile(pair => pair._2 != 1)
      .headOption
      .getOrElse("", 0)
      ._1

  def reverseWords(str: String): String =
    str
      .split("""\s+""")
      .to(LazyList)
      .map(_.reverse)
      .mkString(" ")

  def containsOnlyDigits(str: String): Boolean =
    str.matches("[0-9]+")

  def countVowelsAndConsonants(str: String): (Long, Long) = {
    val vowels = Set('a', 'e', 'i', 'o', 'u')
    str
      .foldLeft((0L, 0L))((acc, char) => {
        val (vowel, consonant) = acc
        if (vowels.contains(char)) {
          (vowel + 1, consonant)
        } else {
          (vowel, consonant + 1)
        }
      })
  }

  def countOccurrencesOfACertainCharacter(str: String, char: Char): Int =
    str.count(ch => ch === char)
}
