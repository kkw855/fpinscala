package com.endsoul.fp.scala
package book.coding_problems

import MyString._

import cats.implicits._

//noinspection SpellCheckingInspection
class StringTest extends UnitSpec {
  "StringTest" should "manipulate the string" in {
    // UTF-8 인코딩
    // 0000 0000 0010 1001
    "A".length shouldBe 1

    // UTF-32 인코딩
    // 0001 1111 0000 1010 0001 (0x1F0A1)
    val uni = "\uD83C\uDCA1"
    uni.length shouldBe 2
    uni.codePointAt(0) shouldBe 0x1F0A1
    // Low-Surrogate value
    uni.codePointAt(1) shouldBe 0xDCA1

    // ASCII: char(0)
    // UNICODE: char(1) Surrogate pair
    val array = Character.toChars(uni.codePointAt(0))
    array.length shouldBe 2
    // High-Surrogate value
    array(0) shouldBe Integer.parseInt("1101100000111100", 2)
    // Low-Surrogate value
    array(1) shouldBe Integer.parseInt("1101110010100001", 2)

    // 0x10000 보다 크면 2 아니면 1 리턴
    Character.charCount(uni.codePointAt(0)) shouldBe 2
    Character.charCount(uni.codePointAt(1)) shouldBe 1

    // UTF-16(Array[Char]) => String 변환
    val str = String.valueOf(array)
    str shouldBe "🂡"

    // 0x1F0A1 0x10400
    val codePoints = "🂡𐐀".codePoints.toArray
    codePoints.length shouldBe 2
    codePoints(0) shouldBe 0x1F0A1
    codePoints(1) shouldBe 0x10400
  }

  // 1. Counting duplicate characters
  it should "count duplicate characters" in {
    countDuplicateCharacters("aa🂡b🂡𐐀c𐐀") shouldBe Map("a" -> 2,
                                                          "🂡" -> 2,
                                                          "b" -> 1,
                                                          "𐐀" -> 2,
                                                          "c" -> 1)
    countDuplicateCharacters("") shouldBe Map.empty
  }

  // 2. Finding the first non-repeated character
  it should "find the first non-repeated character" in {
    firstNonRepeatedCharacter("a🂡b𐐀ccba𐐀123") shouldBe "🂡"
    firstNonRepeatedCharacter("") shouldBe ""
  }

  // 3. Reversing letters and words
  it should "reverse letters and words" in {
    reverseWords("1234 56 7     8") shouldBe "4321 65 7 8"
  }

  // 4. Checking whether a string contains only digits
  it should "checking whether a string contains only digits" in {
    containsOnlyDigits("012346789") shouldBe true
    containsOnlyDigits("1 2") shouldBe false
    containsOnlyDigits("+1") shouldBe false
  }

  // 5. Counting vowels and consonants
  it should "count vowels and consonants" in {
    countVowelsAndConsonants("abcdefghijklmn") shouldBe (3, 11)
  }

  // 6. Counting the occurrences of a certain character
  it should "count the occurrences of a certain character" in {
    countOccurrencesOfACertainCharacter("aa5555bb", '5') shouldBe 4
  }

  // 7. Converting a string into an int, long, float, or double
  it should "converting a string into an int, long, float, or double" in {
    Either.catchOnly[NumberFormatException](Integer.valueOf("123")) shouldBe 123
      .asRight[NumberFormatException]
  }
}
