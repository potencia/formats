package com.potenciasoftware.formats

trait Implicits {

  extension (ss: Iterable[String])(using fmt: ParagraphFormat) {
    def asParagraphs: Iterable[String] = fmt(ss)
  }

  extension (s: String)(using fmt: FormatWithContext) {
    def withContext(
        context: Iterable[(Int, Iterable[String])]
    ): Iterable[String] = fmt(s, context)
  }
}
