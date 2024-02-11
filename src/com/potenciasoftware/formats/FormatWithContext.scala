package com.potenciasoftware.formats

import scala.annotation.tailrec

import FormatWithContext._

/**
  * Given a source text and a sequence of contextual details tied to specific
  * positions in the source text, format the source text and include the context
  * details below each relevant line. The position in the source text and the
  * the details will be connected with lines.
  *
  * @param maxColumn
  *   The maximum column a line can reach before being wrapped
  * @param detailGrouping
  *   How many individual source positions to show context for. If there are
  *   more positions for a given line, that line will be repeated with the
  *   additional context shown.
  */
final case class FormatWithContext(
  maxColumn: Int = 80,
  detailGrouping: Int = 6,
) {

  /** Apply formatting to the input strings */
  def apply(
      text: String,
      context: Iterable[(Int, Iterable[String])],
  ): Iterable[String] = {

    val allContext = context
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).toSeq.distinct)
      .toSeq.sortBy(_._1)

    implicit val format = ParagraphFormat(
      maxColumn = maxColumn,
      blanksBetweenLines = 0,
      trimLines = false,
    )

    val split = text.split("\n").toSeq
    val original = calcLinesAbsPosition((split.headOption.toSeq ++ split.tail.map(" " + _)).asParagraphs).toSeq
    (original.map(_._2) ++ allContext.lastOption.map(_._1 + 1).toSeq)
      .sliding(2).toSeq
      .zipWithIndex.flatMap { case (Seq(start, end), i) =>
        val text = original(i)._1
        val localContext = allContext
          .collect {
            case (pos, paragraphs) if pos >= start && pos < end =>
              (pos - start) -> paragraphs
          }

        Some(localContext)
          .filter(_.nonEmpty).map(
            _.grouped(detailGrouping)
              .map(Details(_, maxColumn))
              .flatMap { details =>
                text +: (0 to (details.end)).map(details.apply)
              }
          )
          .getOrElse(Seq(text))
      }
  }

  /** An alias for apply(String, Iterable[(Int, Iterable[String])]) */
  def format(
      text: String,
      context: Iterable[(Int, Iterable[String])],
  ): Iterable[String] =
    apply(text, context)
}

object FormatWithContext {

  private type Paragraph = Iterable[String]
  private type Paragraphs = Iterable[Paragraph]
  private type Position = Int
  private type ContextualDetails = (Position, Paragraphs)

  @tailrec
  private def calcLinesAbsPosition(input: Iterable[String], abs: Seq[Int] = Seq.empty): Iterable[(String, Int)] = {
    if (input.size == abs.size) input zip abs
    else {
      val view = input.view
      val prior = view.take(abs.size)
      val next = view.drop(abs.size)
      val previousPos = abs.lastOption
        .foldLeft(next.head.takeWhile(_.isWhitespace).size)(_ + _)
      calcLinesAbsPosition(
        prior.toSeq ++ (next.head.dropWhile(_.isWhitespace) +: next.tail.toSeq),
        abs :+ prior.lastOption.map(_.length).getOrElse(0) + previousPos,
      )
    }
  }

  private case class Details(details: Vector[Detail]) {

    private lazy val maxBeam: Int = details.flatMap(_.beams).max

    def apply(lineNum: Int): String = {
      val buf = Array.fill(maxBeam + 1)(' ')
      details.foreach(_.addCornersHorizAndBeam(lineNum, buf))
      val margin = buf.reverse.dropWhile(_ == ' ').reverse.mkString
      details
        .flatMap(_.contextLineAt(lineNum))
        .headOption
        .foldLeft(margin) {
          case (m, (beam, line)) => m.padTo(beam, ' ') + line
        }
    }

    lazy val end: Int = details
      .lastOption
      .map(d => d.paragraphsStart + d.allContext.size)
      .get
  }

  private object Details {

    def apply(details: Seq[ContextualDetails], maxColumn: Int) = {

      val initial = details
        .map(_._1)
        .sorted(Ordering[Int].reverse)
        .tails
        .zipWithIndex
        .map(Detail.create(details.size))
        .filter(_.nonEmpty)
        .toVector

      val withTurns = initial.sorted.zipWithIndex.map {
        case (l, i) => l.copy(turns = i +: l.turns)
      }

      val dataAdjusted = adjustLineData(withTurns)

      val withParagraphs =
        for {
          d <- dataAdjusted.sortBy(_.position)
          (_, paragraphs) <- details.find(_._1 == d.position)
        } yield d.addParagraphs(maxColumn, paragraphs)
      new Details(adjustParagraphsStart(withParagraphs))
    }

    @tailrec
    private def adjustLineData(dd: Vector[Detail]): Vector[Detail] = {
      val toFix = (
        for {
          (d, i) <- dd.view.zipWithIndex
          o <- dd.view.drop(i + 1)
          (collides, normal) = d collidesWith o
          if collides
        } yield (d, i, normal)
      ).headOption

      if (toFix.isEmpty) dd
      else {

        val Some((detail, i, normal)) = toFix: @unchecked
        val maxTurn = dd.flatMap(_.turns).max
        val collisionPoints =
          (i to maxTurn flatMap { lineToCheck =>
            dd.flatMap(_.collisionPointsAt(lineToCheck))
          }).toSet

        adjustLineData(if (normal) {
          val toMove = detail.beams.head
          val sign = if (detail.position > toMove) 1 else -1
          val available =
            ((0 to (dd.size * 2)).toSet -- collisionPoints).toSeq
              .map(_ - toMove)
              .sortBy(d => (Math.abs(d), Math.signum(d.toFloat) * sign))
          val adjutment = available.head
          val newLine = detail.copy(
            beams = Vector(toMove, (toMove + adjutment)) ++ detail.beams.tail,
            turns = detail.turns :+ (maxTurn + 1)
          )
          dd.updated(i, newLine)
        } else dd.map(l => l.copy(paragraphsStart = l.paragraphsStart + 1)))
      }
    }

    @tailrec
    private def adjustParagraphsStart(dd: Vector[Detail], pos: Int = 0): Vector[Detail] = {

      val prior = dd.take(pos)
      val view = dd.view.drop(pos)
      val d = view.head
      val tail = view.tail

      val currentStart = d.paragraphsStart
      val lastContextEnd = prior.lastOption.map(_.end).getOrElse(1)

      val target = currentStart max (lastContextEnd + 2) max (d.turns.last + 1)
      val moveDetailsStart: Detail => Detail =
        d => d.copy(paragraphsStart = d.paragraphsStart + (target - currentStart))

      if (pos > dd.size - 2)
        prior :+ moveDetailsStart(d)
      else
        adjustParagraphsStart(prior ++ (moveDetailsStart(d) +: (tail.map(moveDetailsStart).toVector)), pos + 1)
    }
  }

  private case class Detail(
      positions: List[Int],
      beams: Vector[Int],
      turns: Vector[Int],
      paragraphsStart: Int,
      paragraphs: Paragraphs
  ) {

    lazy val nonEmpty = positions.nonEmpty

    lazy val position = positions.head
    lazy val otherPositions = positions.tail

    def addParagraphs(maxColumn: Int, input: Paragraphs): Detail = {
      implicit val format =
        ParagraphFormat(
          maxColumn = maxColumn - beams.last - 3,
          blanksBetweenLines = 0,
        )

      copy(paragraphs = input.map(_.asParagraphs))
    }

    def cornersAt(lineNum: Int): Option[Seq[Int]] =
      Some(turns.indexOf(lineNum)).filter(_ > -1) map { turn =>
        beams.drop(turn) take (2)
      }

    def beamAt(lineNum: Int): Int =
      beams(turns.size - turns.dropWhile(lineNum > _).size)

    def collisionPointsAt(lineNum: Int): Set[Int] =
      (cornersAt(lineNum).toSeq.flatten :+ beamAt(lineNum)).toSet

    def collidesWith(other: Detail): (Boolean, Boolean) = {
      def maxTurn = (turns ++ other.turns).max
      if (
        (0 to maxTurn) exists { n =>
          (collisionPointsAt(n) intersect other.collisionPointsAt(n)).nonEmpty
        }
      ) (true, true)
      else {
        val specialCollision =
          (paragraphsStart to other.turns.max) exists { lineNum =>
            other.cornersAt(lineNum) exists { corners =>
              corners.exists(beams.last <= _)
            }
          }
        if (specialCollision) (true, false) else (false, false)
      }
    }

    def addCornersHorizAndBeam(lineNum: Int, buf: Array[Char]): Unit =
      cornersAt(lineNum) match {
        case Some(ft) =>
          val Seq(from, to) = ft
          if (from < to) {
            buf(from) = '╰'
            buf(to) = '╮'
            ((from + 1) until to).filter(buf(_) == ' ').foreach { buf(_) = '─' }
          } else if (to < from) {
            buf(from) = '╯'
            buf(to) = '╭'
            ((to + 1) until from).filter(buf(_) == ' ').foreach { buf(_) = '─' }
          } else buf(to) = '│'
        case None =>
          if (lineNum < paragraphsStart)
            buf(beamAt(lineNum)) = '│'
      }

    lazy val allContext = {
      val lastGraf = paragraphs.size - 1
      paragraphs.zipWithIndex flatMap { case (paragraph, i) =>
        if (i == lastGraf)
          paragraph.take(1).map("╰─ " + _) ++
            paragraph.drop(1).map("   " + _)
        else
          paragraph.take(1).map("├─ " + _) ++
            paragraph.drop(1).map("│  " + _) ++
            Seq("│")
      }
    }

    lazy val end: Int = paragraphsStart + allContext.size - 1

    def contextLineAt(lineNum: Int): Option[(Int, String)] =
      (lineNum match {
        case i if i < paragraphsStart => None
        case i => allContext.drop(i - paragraphsStart).headOption
      }).map(beams.last -> _)
  }

  private object Detail {

    def create(groupSize: Int)(t: (Seq[Int], Int)): Detail =
      Detail(
        t._1.toList,
        beams = t._1.headOption.toVector :+ (t._2 * 2 + 1),
        turns = Vector.empty,
        ((groupSize - t._2) * 2 - 1),
        Nil,
      )

    implicit val ordering: Ordering[Detail] =
      new Ordering[Detail] {
        override def compare(x: Detail, y: Detail): Int = {
          if (y.beams.exists(_ < x.position)) 0 else -1
        }
      }.orElse(Ordering.by[Detail, Int](_.position).reverse)
  }
}
