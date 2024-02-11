package com.potenciasoftware.formats

import scala.io.Source

object TestUtils {
  def resourceWith[A](name: String)(f: Source => A): A = {
    val source = Source.fromResource(name)
    try f(source)
    finally source.close()
  }
}
