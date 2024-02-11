package com.potenciasoftware.formats

import com.potenciasoftware.formats.TestUtils._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParagraphFormatTest extends AnyFlatSpec with Matchers {

  "ParagraphFormat" should "break lines longer than maxColumn" in {
    val lines = resourceWith("lorum.txt")(_.getLines().drop(1).take(2).toSeq)
    ParagraphFormat(blanksBetweenLines = 0)(lines).mkString("\n") shouldBe
      """|Sed condimentum sem libero, ut tempus orci rutrum pulvinar. Fusce scelerisque
         |sem ut leo tincidunt, vitae volutpat erat euismod. Curabitur neque urna,
         |pharetra placerat nulla nec, egestas scelerisque lorem. Maecenas congue lorem
         |vel eros volutpat consectetur. Ut convallis pellentesque viverra. In non lacinia
         |tortor. Quisque ac tellus eu velit porttitor varius. Donec viverra erat ac quam
         |volutpat, vitae rhoncus justo consectetur. Phasellus laoreet dolor sed semper
         |tempus.
         |Phasellus porta risus id imperdiet bibendum. Nunc blandit in magna eget lacinia.
         |Curabitur ultricies ante eu velit mollis imperdiet. Quisque suscipit finibus
         |quam sodales auctor. Nunc blandit maximus venenatis. Curabitur at nisl ac arcu
         |porta pharetra. Integer pellentesque posuere fermentum. Cras quis faucibus orci.
         |Nam sed laoreet mauris. Ut eget nibh sed dui egestas malesuada. Curabitur at
         |semper dolor. Proin molestie dapibus tincidunt. Nunc luctus blandit volutpat.
         |Aliquam et libero dolor. Aliquam erat volutpat.""".stripMargin
  }

  it should "insert blank lines between paragraphs" in {
    val lines = resourceWith("lorum.txt")(_.getLines().take(3).toSeq)
    ParagraphFormat().format(lines).mkString("\n") shouldBe
      """|Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas ligula dui,
         |interdum id lorem vitae, bibendum facilisis elit. Proin mauris lorem, aliquet
         |sed suscipit nec, faucibus in nisl. Suspendisse ut neque neque. Integer erat
         |diam, finibus sit amet velit in, ornare tempor diam. Morbi consectetur dui et
         |est feugiat, ac condimentum tortor scelerisque. Quisque nec lobortis ligula. Sed
         |id leo nunc.
         |
         |Sed condimentum sem libero, ut tempus orci rutrum pulvinar. Fusce scelerisque
         |sem ut leo tincidunt, vitae volutpat erat euismod. Curabitur neque urna,
         |pharetra placerat nulla nec, egestas scelerisque lorem. Maecenas congue lorem
         |vel eros volutpat consectetur. Ut convallis pellentesque viverra. In non lacinia
         |tortor. Quisque ac tellus eu velit porttitor varius. Donec viverra erat ac quam
         |volutpat, vitae rhoncus justo consectetur. Phasellus laoreet dolor sed semper
         |tempus.
         |
         |Phasellus porta risus id imperdiet bibendum. Nunc blandit in magna eget lacinia.
         |Curabitur ultricies ante eu velit mollis imperdiet. Quisque suscipit finibus
         |quam sodales auctor. Nunc blandit maximus venenatis. Curabitur at nisl ac arcu
         |porta pharetra. Integer pellentesque posuere fermentum. Cras quis faucibus orci.
         |Nam sed laoreet mauris. Ut eget nibh sed dui egestas malesuada. Curabitur at
         |semper dolor. Proin molestie dapibus tincidunt. Nunc luctus blandit volutpat.
         |Aliquam et libero dolor. Aliquam erat volutpat.""".stripMargin
  }

  it should "enable the implicit Iterable[String].asParagraphs" in {
    val lines = resourceWith("lorum.txt")(_.getLines().drop(4).take(3).toSeq)
    implicit val fmt = ParagraphFormat()
    lines.asParagraphs.mkString("\n") shouldBe
    """|Aenean eget lectus risus. Aliquam non porttitor lectus. Praesent imperdiet nulla
       |non semper elementum. Proin suscipit, velit blandit cursus pharetra, turpis
       |ligula vestibulum sapien, sit amet dignissim tellus mi et dui. Donec interdum
       |nisl nec massa pulvinar dictum. Duis pharetra ex posuere, molestie nibh non,
       |ornare nisl. Pellentesque habitant morbi tristique senectus et netus et
       |malesuada fames ac turpis egestas. Etiam nec justo ac sapien bibendum bibendum.
       |Morbi nec velit laoreet, egestas nisl sed, lobortis ex. Ut tincidunt massa at
       |sapien dignissim porttitor. Duis porttitor est pharetra sem tincidunt, et
       |tincidunt nisl ultrices. Praesent enim sapien, finibus eget dignissim sed,
       |sagittis vel ex. Mauris vitae elementum nibh.
       |
       |Sed faucibus nisl nec sagittis auctor. Aliquam tincidunt neque id augue aliquet,
       |quis condimentum purus dignissim. Pellentesque habitant morbi tristique senectus
       |et netus et malesuada fames ac turpis egestas. Aenean non massa ut odio sagittis
       |semper non blandit velit. Sed blandit in massa vel scelerisque. Praesent sit
       |amet turpis faucibus, iaculis orci non, vehicula nunc. In luctus, tellus in
       |tincidunt pharetra, metus tortor semper diam, eu semper enim libero nec odio.
       |
       |Aliquam at mattis tellus. Vestibulum metus nunc, dapibus ac consequat at,
       |ullamcorper quis massa. Aliquam vel magna venenatis, vehicula dui eu, gravida
       |ligula. Sed nec arcu ut tellus ultrices iaculis. Aliquam tellus dui, convallis
       |et interdum fermentum, volutpat sit amet est. Fusce faucibus justo et turpis
       |ullamcorper, ut fringilla nulla mattis. Phasellus at nisi justo. Praesent quis
       |fringilla erat, lobortis rutrum quam. Ut ut cursus urna. Proin faucibus ipsum
       |massa, eu imperdiet leo iaculis eu. Nullam augue turpis, convallis in nibh sit
       |amet, feugiat vestibulum urna.""".stripMargin
  }
}
