// build.sbt:
// scalaVersion := "2.11.0
//
// libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0-M7"
//
import scalaz._
import Scalaz._

/**
 * Scalaz port of Ed Kmett's code in http://comonad.com/reader/2008/rotten-bananas/
 *
 * I believe this illustrates functor correspondence with the "transform" associative 
 * binary relation of type parameters in higher order types, defined as:
 * {{{
     b + - i
   b b b b i
   + b + - i
   - b - + i
   i i i i i
   }}}
 */
object variance {

  /** Boxed newtype for F[G[A]]. */
  case class Nested[F[_], G[_], A](value: F[G[A]])

  /** Nested covariant functors yield a covariant functor. */
  implicit def `+[+] = +`[F[_]: Functor, G[_]: Functor]: Functor[({type l[a] = Nested[F, G, a]})#l] =
    new Functor[({type l[a] = Nested[F, G, a]})#l] {
      def map[A, B](nested: Nested[F, G, A])(f: A => B): Nested[F, G, B] = {
        val fga = nested.value
        val fgb = fga.map((ga: G[A]) => ga.map(f))
        Nested(fgb)
      }
    }

  /** Contravariant functor in a covariant functor yields a contravariant functor. */
  implicit def `+[-] = -`[F[_]: Functor, G[_]: Contravariant]: Contravariant[({type l[a] = Nested[F, G, a]})#l] =
    new Contravariant[({type l[a] = Nested[F, G, a]})#l] {
      def contramap[A, B](nested: Nested[F, G, A])(f: B => A): Nested[F, G, B] = {
        val fga = nested.value
        val fgb = fga.map((ga: G[A]) => ga.contramap(f))
        Nested(fgb)
      }
    }

  /** Covariant functor in a contravariant functor yields a contravariant functor. */
  implicit def `-[+] = -`[F[_]: Contravariant, G[_]: Functor]: Contravariant[({type l[a] = Nested[F, G, a]})#l] =
    new Contravariant[({type l[a] = Nested[F, G, a]})#l] {
      def contramap[A, B](nested: Nested[F, G, A])(f: B => A): Nested[F, G, B] = {
        val fga = nested.value
        val fgb = fga.contramap((gb: G[B]) => gb.map(f))
        Nested(fgb)
      }
    }

  /** Nested contravariant functors yield a covariant functor. */
  implicit def `-[-] = +`[F[_]: Contravariant, G[_]: Contravariant]: Functor[({type l[a] = Nested[F, G, a]})#l] =
    new Functor[({type l[a] = Nested[F, G, a]})#l] {
      def map[A, B](nested: Nested[F, G, A])(f: A => B): Nested[F, G, B] = {
        val fga = nested.value
        val fgb = fga.contramap((gb: G[B]) => gb.contramap(f))
        Nested(fgb)
      }
    }

  /** Covariant functor in an invariant functor yields an invariant functor. */
  implicit def `i[+] = i`[F[_]: InvariantFunctor, G[_]: Functor]: InvariantFunctor[({type l[a] = Nested[F, G, a]})#l] =
    new InvariantFunctor[({type l[a] = Nested[F, G, a]})#l] {
      def xmap[A, B](nested: Nested[F, G, A], f: A => B, g: B => A): Nested[F, G, B] = {
        val fga = nested.value
        val fgb: F[G[B]] = fga.xmap((ga: G[A]) => ga.map(f), (gb: G[B]) => gb.map(g))
        Nested(fgb)
      }
    }

  /** Contravariant functor in an invariant functor yields an invariant functor. */
  implicit def `i[-] = i`[F[_]: InvariantFunctor, G[_]: Contravariant]: InvariantFunctor[({type l[a] = Nested[F, G, a]})#l] =
    new InvariantFunctor[({type l[a] = Nested[F, G, a]})#l] {
      def xmap[A, B](nested: Nested[F, G, A], f: A => B, g: B => A): Nested[F, G, B] = {
        val fga = nested.value
        val fgb: F[G[B]] = fga.xmap((ga: G[A]) => ga.contramap(g), (gb: G[B]) => gb.contramap(f))
        Nested(fgb)
      }
    }

  /** Invariant functor in a covariant functor yields an invariant functor. */
  implicit def `+[i] = i`[F[_]: Functor, G[_]: InvariantFunctor]: InvariantFunctor[({type l[a] = Nested[F, G, a]})#l] =
    new InvariantFunctor[({type l[a] = Nested[F, G, a]})#l] {
      def xmap[A, B](nested: Nested[F, G, A], f: A => B, g: B => A): Nested[F, G, B] = {
        val fga = nested.value
        val fgb: F[G[B]] = fga.map((ga: G[A]) => ga.xmap(f, g))
        Nested(fgb)
      }
    }

  /** Invariant functor in a contravariant functor yields an invariant functor. */
  implicit def `-[i] = i`[F[_]: Contravariant, G[_]: InvariantFunctor]: InvariantFunctor[({type l[a] = Nested[F, G, a]})#l] =
    new InvariantFunctor[({type l[a] = Nested[F, G, a]})#l] {
      def xmap[A, B](nested: Nested[F, G, A], f: A => B, g: B => A): Nested[F, G, B] = {
        val fga = nested.value
        val fgb: F[G[B]] = fga.contramap((gb: G[B]) => gb.xmap(g, f))
        Nested(fgb)
      }
    }

  /** Invariant functor in an invariant functor yields an invariant functor. */
  implicit def `i[i] = i`[F[_]: InvariantFunctor, G[_]: InvariantFunctor]: InvariantFunctor[({type l[a] = Nested[F, G, a]})#l] =
    new InvariantFunctor[({type l[a] = Nested[F, G, a]})#l] {
      def xmap[A, B](nested: Nested[F, G, A], f: A => B, g: B => A): Nested[F, G, B] = {
        val fga = nested.value
        val fgb: F[G[B]] = fga.xmap((ga: G[A]) => ga.xmap(f, g), (gb: G[B]) => gb.xmap(g, f))
        Nested(fgb)
      }
    }

  // This is likely incorrect because bivariant[invariant] yields bivariant but it should yield invariant.
  trait Bivariant[F[_]] {
    def tmap[A, B](fa: F[A]): F[B]
  }

  implicit class BivariantSyntax[F[_], A](val fa: F[A]) {
    def tmap[B](implicit B: Bivariant[F]): F[B] = B.tmap[A, B](fa)
  }

  /** Any type constructor in a bivariant functor yields a bivariant functor. */
  implicit def `b[*] = b`[F[_]: Bivariant, G[_]]: Bivariant[({type l[a] = Nested[F, G, a]})#l] =
    new Bivariant[({type l[a] = Nested[F, G, a]})#l] {
      def tmap[A, B](nested: Nested[F, G, A]): Nested[F, G, B] = {
        val fga = nested.value
        val fgb = fga.tmap[G[B]]
        Nested(fgb)
      }
    }

  /** Bivariant functor in a covariant functor yields a bivariant functor. */
  implicit def `+[b] = b`[F[_]: Functor, G[_]: Bivariant]: Bivariant[({type l[a] = Nested[F, G, a]})#l] =
    new Bivariant[({type l[a] = Nested[F, G, a]})#l] {
      def tmap[A, B](nested: Nested[F, G, A]): Nested[F, G, B] = {
        val fga = nested.value
        val fgb = fga.map((ga: G[A]) => ga.tmap[B])
        Nested(fgb)
      }
    }

  /** Bivariant functor in a contravariant functor yields a bivariant functor. */
  implicit def `-[b] = b`[F[_]: Contravariant, G[_]: Bivariant]: Bivariant[({type l[a] = Nested[F, G, a]})#l] =
    new Bivariant[({type l[a] = Nested[F, G, a]})#l] {
      def tmap[A, B](nested: Nested[F, G, A]): Nested[F, G, B] = {
        val fga = nested.value
        val fgb = fga.contramap((gb: G[B]) => gb.tmap[A])
        Nested(fgb)
      }
    }


}
