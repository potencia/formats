package com.potenciasoftware.formats

trait Implicits {

  implicit class AsParagraphsOp(
      ss: Iterable[String]
  )(implicit
      fmt: ParagraphFormat
  ) {
    def asParagraphs: Iterable[String] = fmt(ss)
  }

  implicit class WithContextOp(
      s: String
  )(implicit
      fmt: FormatWithContext
  ) {
    def withContext(
        context: Iterable[(Int, Iterable[String])]
    ): Iterable[String] = fmt(s, context)
  }
}
