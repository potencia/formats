package com.potenciasoftware.formats

import com.potenciasoftware.formats.TestUtils._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FormatWithContextTest extends AnyFlatSpec with Matchers {

  "FormatWithContext" should "combine text with positional context notes" in {
    val (text, context) = loadVerses
    FormatWithContext().format(text, context).mkString("\n") shouldBe
      """|Hebrews 1:1 Long ago, at many times and in many ways, God spoke to our fathers
         |╰──╮                                                      │
         | ╭─│──────────────────────────────────────────────────────╯
         | │ ╰─ English Standard Version
         | │
         | ╰─ Lit. after He spoke
         |
         |by the prophets,
         |╰──╮   │
         | ╭─│───╯
         | │ ├─ Lit. in
         | │ ╰─ This denotes that God possessed their hearts, controlled their minds,
         | │    ordered their tongues, so that they spake not their own words, but His
         | │    words.
         | │    --Arthur Pink
         | │
         | ╰─ one who, moved by the Spirit of God and hence his organ or spokesman,
         |    solemnly declares to men what he has received by inspiration, especially
         |    concerning future events, and in particular such as relate to the cause and
         |    kingdom of God and to human salvation
         |
         |Hebrews 1:2 but in these last days he has spoken to us by his Son, whom he
         | ╭─────────────────────────────────────────────────────╯
         | ├─ Lit. in
         | ╰─ The whole revelation and manifestation of God is now in Christ; He alone
         |    reveals the Father's heart. It is not only that Christ declared or delivered
         |    God's message, but that He himself was and is God's message.
         |    --Arthur Pink
         |
         |appointed the heir of all things, through whom also he created the world.""".stripMargin
  }

  it should "enable the implicit String.withContext" in {
    implicit val format = FormatWithContext(
      maxColumn = 40,
      detailGrouping = 1
    )

    val (text, context) = loadVerses
    text.withContext(context).mkString("\n") shouldBe
      """|Hebrews 1:1 Long ago, at many times and
         |╰╮
         | ╰─ English Standard Version
         |
         |in many ways, God spoke to our fathers
         | ╭────────────────╯
         | ╰─ Lit. after He spoke
         |
         |by the prophets,
         |╰╮
         | ├─ Lit. in
         | ╰─ This denotes that God possessed
         |    their hearts, controlled their
         |    minds, ordered their tongues, so
         |    that they spake not their own words,
         |    but His words.
         |    --Arthur Pink
         |
         |by the prophets,
         | ╭─────╯
         | ╰─ one who, moved by the Spirit of God
         |    and hence his organ or spokesman,
         |    solemnly declares to men what he has
         |    received by inspiration, especially
         |    concerning future events, and in
         |    particular such as relate to the
         |    cause and kingdom of God and to
         |    human salvation
         |
         |Hebrews 1:2 but in these last days he
         |has spoken to us by his Son, whom he
         | ╭───────────────╯
         | ├─ Lit. in
         | ╰─ The whole revelation and
         |    manifestation of God is now in
         |    Christ; He alone reveals the
         |    Father's heart. It is not only that
         |    Christ declared or delivered God's
         |    message, but that He himself was and
         |    is God's message.
         |    --Arthur Pink
         |
         |appointed the heir of all things,
         |through whom also he created the
         |world.""".stripMargin
  }

  it should "allow the space between points and groups to be configured" in {
    implicit val format = FormatWithContext(
      blanksBetweenPositionPoints = 2,
      blanksBetweenPositionGroups = 3,
    )

    val (text, context) = loadVerses
    text.withContext(context).mkString("\n") shouldBe
      """|Hebrews 1:1 Long ago, at many times and in many ways, God spoke to our fathers
         |╰──╮                                                      │
         | ╭─│──────────────────────────────────────────────────────╯
         | │ ╰─ English Standard Version
         | │
         | │
         | │
         | ╰─ Lit. after He spoke
         |
         |by the prophets,
         |╰──╮   │
         | ╭─│───╯
         | │ ├─ Lit. in
         | │ │
         | │ │
         | │ ╰─ This denotes that God possessed their hearts, controlled their minds,
         | │    ordered their tongues, so that they spake not their own words, but His
         | │    words.
         | │    --Arthur Pink
         | │
         | │
         | │
         | ╰─ one who, moved by the Spirit of God and hence his organ or spokesman,
         |    solemnly declares to men what he has received by inspiration, especially
         |    concerning future events, and in particular such as relate to the cause and
         |    kingdom of God and to human salvation
         |
         |Hebrews 1:2 but in these last days he has spoken to us by his Son, whom he
         | ╭─────────────────────────────────────────────────────╯
         | ├─ Lit. in
         | │
         | │
         | ╰─ The whole revelation and manifestation of God is now in Christ; He alone
         |    reveals the Father's heart. It is not only that Christ declared or delivered
         |    God's message, but that He himself was and is God's message.
         |    --Arthur Pink
         |
         |appointed the heir of all things, through whom also he created the world.""".stripMargin
  }

  def loadVerses = resourceWith("verses.txt") { r =>
    val lines = r.getLines()
    (
      lines.next().replaceAll("\\|", "\n"),
      lines.toSeq.map { l =>
        val Array(k, vv @ _*) = l.split("\\|"): @unchecked
        k.toInt -> vv.toSeq
      }
    )
  }

  val nonZero: Seq[String] = Seq(
    "One",
    "Two",
    "Three",
    "Four",
    "Five",
    "Six",
    "Seven",
    "Eight",
    "Nine",
  )

  val teens: Seq[String] = Seq(
    "Ten",
    "Eleven",
    "Twelve",
    "Thirteen",
    "Fourteen",
    "Fifteen",
    "Sixteen",
    "Seventeen",
    "Eighteen",
    "Nineteen",
  )

  val tensPlace =
    Seq(
      "Twenty",
      "Thirty",
      "Fourty",
    ) flatMap { zero =>
      zero +: nonZero.map(zero + "-" + _.toLowerCase)
    }

  val testDetails: Seq[Iterable[String]] =
    (("Zero" +: nonZero) ++ teens ++ (tensPlace :+ "Fifty")).map(Seq(_))

  it should "work for troublesome combinations" in {

    val testSize = 4
    val fmt = FormatWithContext(detailGrouping = testSize)

    val troublesomeCombos = Seq(
      Seq(0, 1, 2, 3),
      Seq(2, 3, 5, 6),
      Seq(5, 8, 10, 12),
      Seq(6, 8, 11, 13),
    ).map(_.map(p => p -> testDetails(p)))

    def test(a: Int): String =
      fmt("", troublesomeCombos(a)).mkString("\n")

    test(0) shouldBe
      """|
         |╰│││───╮
         | ╰││─╮ ╰─ Zero
         | ╭│╯ │
         | │╰╮ ╰─ One
         | │ │
         | │ ╰─ Two
         | │
         | ╰─ Three
         |""".stripMargin

    test(1) shouldBe
      """|
         | ╭││─│╯
         | │╰│─│─╮
         | │ ╰╮│ │
         | │ ╭│╯ ╰─ Two
         | │ │╰╮
         | │ │ ╰─ Three
         | │ │
         | │ ╰─ Five
         | │
         | ╰─ Six
         |""".stripMargin

    test(2) shouldBe
      """|
         | ╭───│──│─│─╯
         | │ ╭─│──│─╯
         | │ │ ╰─╮│
         | │ │ ╭─│╯
         | │ │ │ ╰─ Five
         | │ │ │
         | │ │ ╰─ Eight
         | │ │
         | │ ╰─ Ten
         | │
         | ╰─ Twelve
         |""".stripMargin

    test(3) shouldBe
      """|
         | ╭────│─│──│─╯
         | │ ╭──│─│──╯
         | │ │ ╭│─╯
         | │ │ │╰╮
         | │ │ │ ╰─ Six
         | │ │ │
         | │ │ ╰─ Eight
         | │ │
         | │ ╰─ Eleven
         | │
         | ╰─ Thirteen
         |""".stripMargin
  }
}
