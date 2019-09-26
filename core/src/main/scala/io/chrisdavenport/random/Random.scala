package io.chrisdavenport.random

import cats.implicits._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import scala.util.{Random => SRandom}

trait Random[F[_]] {

  def betweenDouble(minInclusive: Double, maxExclusive: Double): F[Double]

  def betweenInt(minInclusive: Int, maxExclusive: Int): F[Int]

  def betweenFloat(minInclusive: Float, maxExclusive: Float): F[Float]

  def betweenLong(minInclusive: Long, maxExclusive: Long): F[Long]

  def nextBoolean: F[Boolean]

  def nextBytes(n: Int): F[Array[Byte]]

  def nextDouble: F[Double]

  def nextFloat: F[Float]

  def nextGaussian: F[Double]

  def nextInt: F[Int]

  def nextIntBounded(n: Int): F[Int] = betweenInt(0, n)

  def nextLong: F[Long]

  def nextLongBounded(n: Long): F[Long] = betweenLong(0L, n)

  def nextPrintableChar: F[Char]

  def nextString(length: Int): F[String]

  def shuffleList[A](l: List[A]): F[List[A]]

  def shuffleVector[A](v: Vector[A]): F[Vector[A]]


}

object Random {

  def apply[F[_]](implicit ev: Random[F]): Random[F] = ev

  def nScalaUtilRandom[F[_]: Sync](n: Int): F[Random[F]] = for {
    ref <- Ref[F].of(0)
    array <- Sync[F].delay(Array.fill(n)(new SRandom()))
  } yield {
    def incrGet = ref.modify(i => (if (i < i) i else 0, i))
    def selectRandom = incrGet.map(array(_))
    new ScalaRandom[F](selectRandom) {}
  }

  def scalaUtilRandom[F[_]: Sync]: F[Random[F]] = Sync[F].delay{
    val sRandom = new SRandom()
    new ScalaRandom[F](sRandom.pure[F]){}

  }

  def seedInt[F[_]: Sync](seed: Int): Random[F] = {
    val sRandom = new SRandom(seed)
    new ScalaRandom[F](sRandom.pure[F]){}
  }

  def seedLong[F[_]: Sync](seed: Long): Random[F] = {
    val sRandom = new SRandom(seed)
    new ScalaRandom[F](sRandom.pure[F]){}
  }

  private abstract class ScalaRandom[F[_]: Sync](f: F[SRandom]) extends Random[F]{

    def betweenLong(minInclusive: Long, maxExclusive: Long): F[Long] = for {
      r <- f
      out <- Sync[F].delay(r.between(minInclusive, maxExclusive))
    } yield out

    def betweenInt(minInclusive: Int, maxExclusive: Int): F[Int] = for {
      r <- f
      out <- Sync[F].delay(r.between(minInclusive, maxExclusive))
    } yield out

    def betweenFloat(minInclusive: Float, maxExclusive: Float): F[Float] = for {
      r <- f
      out <- Sync[F].delay(r.between(minInclusive, maxExclusive))
    } yield out

    def betweenDouble(minInclusive: Double, maxExclusive: Double): F[Double] = for {
      r <- f
      out <- Sync[F].delay(r.between(minInclusive, maxExclusive))
    } yield out

    def nextBoolean: F[Boolean] = for {
      r <- f
      out <- Sync[F].delay(r.nextBoolean())
    } yield out

    def nextBytes(n: Int): F[Array[Byte]] = for {
      r <- f
      out <- Sync[F].delay(r.nextBytes(n))
    } yield out

    def nextDouble: F[Double] = for {
      r <- f
      out <- Sync[F].delay(r.nextDouble())
    } yield out

    def nextFloat: F[Float] = for {
      r <- f
      out <- Sync[F].delay(r.nextFloat())
    } yield out

    def nextGaussian: F[Double] = for {
      r <- f
      out <- Sync[F].delay(r.nextGaussian())
    } yield out

    def nextInt: F[Int] = for {
      r <- f
      out <- Sync[F].delay(r.nextInt())
    } yield out


    def nextLong: F[Long] = for {
      r <- f
      out <- Sync[F].delay(r.nextLong())
    } yield out


    def nextPrintableChar: F[Char] = for {
      r <- f
      out <- Sync[F].delay(r.nextPrintableChar())
    } yield out

    def nextString(length: Int): F[String] = for {
      r <- f
      out <- Sync[F].delay(r.nextString(length))
    } yield out

    def shuffleList[A](l: List[A]): F[List[A]] = for {
      r <- f
      out <- Sync[F].delay(r.shuffle(l))
    } yield out

    def shuffleVector[A](v: Vector[A]): F[Vector[A]] = for {
      r <- f
      out <- Sync[F].delay(r.shuffle(v))
    } yield out
  }
}