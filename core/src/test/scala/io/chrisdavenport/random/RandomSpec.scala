package io.chrisdavenport.random

import org.specs2._
import cats.effect._

object RandomSpec extends mutable.Specification with ScalaCheck {

  "Random" should {
    "nextIntBounded" >> prop { _: Unit =>
      val test = for {
        r <- Random.scalaUtilRandom[IO]
        i <- r.nextIntBounded(10)
      } yield {
        i must be_<=(10)
        i must be_>=(0)
      }

      test.unsafeRunSync
    }

    "nextLongBounded" >> prop { _ : Unit =>
      val test = for {
        r <- Random.scalaUtilRandom[IO]
        i <- r.nextLongBounded(10L)
      } yield {
        i must be_<=(10L)
        i must be_>=(0L)
      }

      test.unsafeRunSync
    }

    "betweenInt positive" >> prop { _: Unit =>
      val test = for {
        r <- Random.scalaUtilRandom[IO]
        i <- r.betweenInt(20, 30)
      } yield {
        i must be_<=(29)
        i must be_>=(20)
      }

      test.unsafeRunSync
    }

    "betweenInt negative" >> prop { _: Unit => 
      val test = for {
        r <- Random.scalaUtilRandom[IO]
        i <- r.betweenInt(-100, -1)
      } yield {
        i must be_<=(-2)
        i must be_>=(-100)
      }

      test.unsafeRunSync
    }

    "betweenLong positive" >> prop { _: Unit => 
      val test = for {
        r <- Random.scalaUtilRandom[IO]
        i <- r.betweenLong(20L, 30L)
      } yield {
        i must be_<=(29L)
        i must be_>=(20L)
      }

      test.unsafeRunSync
    }

    "betweenLong negative" >> prop { _: Unit => 
      val test = for {
        r <- Random.scalaUtilRandom[IO]
        i <- r.betweenLong(-100L, -1L)
      } yield {
        i must be_<=(-2L)
        i must be_>=(-100L)
      }

      test.unsafeRunSync
    }
  }

  "ShardedScalaUtilRandom" should {
    "work across multiple calls" >> prop { _: Unit =>
      val test = for {
        r <- Random.scalaUtilRandomN[IO](2)
        i <- r.nextIntBounded(10)
        i2 <- r.nextIntBounded(10)
        i3 <- r.nextIntBounded(10)
        i4 <- r.nextIntBounded(10)
      } yield {
        i must be_<=(10)
        i must be_>=(0)
        i2 must be_<=(10)
        i2 must be_>=(0)
        i3 must be_<=(10)
        i3 must be_>=(0)
        i4 must be_<=(10)
        i4 must be_>=(0)
      }

      test.unsafeRunSync
    }
  }

}