package com.potenciasoftware.formats

import scala.annotation.tailrec
import scala.util.Try

/**
  * Format lines of text into paragraph blocks. Each String in the input
  * Iterable will become a paragraph.
  *
  * @param maxColumn
  *   The maximum column a line can reach before being wrapped
  * @param blanksBetweenLines
  *   How many blank lines to put between paragraphs
  * @param trimLines
  *   Whether to trim any leading whitespace that remains after wrapping long lines
  */
case class ParagraphFormat(
    maxColumn: Int = 80,
    blanksBetweenLines: Int = 1,
    trimLines: Boolean = true,
) {

  /** Apply formatting to the input strings */
  def apply(input: Iterable[String]): Iterable[String] = {

    var before = Option.empty[List[String]]

    @tailrec
    def wrap(rest: String, tail: List[String]): List[String] = {

      val wrapped = before
        .orElse {
          before = Some(List.fill(blanksBetweenLines)(""))
          None
        }
        .filter(_ => tail.isEmpty)
        .foldRight(tail)(_ ::: _)

      if (rest.length < maxColumn) {
        rest :: wrapped
      } else {
        var at = maxColumn
        while (Try(rest.charAt(at)).toOption.forall(!_.isWhitespace)) at -= 1
        val (nextLine, leftOver) = rest.splitAt(at)

        at = 0
        if (trimLines)
          while (Try(leftOver.charAt(at)).toOption.exists(_.isWhitespace)) at += 1
        wrap(leftOver.splitAt(at)._2, nextLine :: wrapped)
      }
    }

    input.flatMap(wrap(_, Nil).reverse)
  }

  /** An alias for apply(Iterable[String]) */
  def format(input: Iterable[String]): Iterable[String] = apply(input)
}
