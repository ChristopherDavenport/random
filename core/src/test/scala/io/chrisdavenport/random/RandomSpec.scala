package io.chrisdavenport.random

import org.specs2._
import cats.effect._

object RandomSpec extends mutable.Specification {

  "Random" should {
    "be unit" in {
      () must be_=== ()
    }
  }

}