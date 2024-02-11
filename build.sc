import mill._, scalalib._, publish._

import $ivy.`com.lihaoyi::mill-contrib-versionfile:`
import contrib.versionfile.VersionFileModule

object formats extends Cross[FormatsModule]("2.13", "3") {
  def defaultCrossSegments = Seq("3")
}

trait FormatsModule
    extends CrossScalaModule
    with VersionFileModule
    with PublishModule {

  def millSourcePath = super.millSourcePath / os.up
  def publishVersion = currentVersion.map(_.toString)

  def sonatypeUri = "https://s01.oss.sonatype.org/service/local"

  def pomSettings = T {
    PomSettings(
      description = "Powerful Formats",
      organization = "com.potenciasoftware",
      url = s"https://github.com/potencia/formats",
      licenses = Seq(License.MIT.copy(url =
        s"https://raw.githubusercontent.com/potencia/formats/master/LICENSE"
      )),
      versionControl = VersionControl.github("potencia", "formats"),
      developers = Seq(Developer(
        id = "johnsonjii",
        name = "John Johnson II",
        url = "https://github.com/johnsonjii",
      )),
    )
  }

  def scalaVersion = crossValue match {
    case "2.13" => "2.13.12"
    case "3" => "3.3.1"
  }

  def scalacOptions = Seq(
    "-encoding",
    "utf8",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Wunused:imports",
    "-Wunused:privates",
    "-Wunused:locals",
  ) ++ Option
    .when(crossValue == "3")(Seq(
      "-no-indent",
      "-old-syntax",
    )).toSeq.flatten

  object test extends ScalaTests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.17")
  }
}
