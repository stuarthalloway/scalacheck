/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2008 Rickard Nilsson. All rights reserved.          **
**  http://code.google.com/p/scalacheck/                                   **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck

import scala.collection.mutable.ListBuffer
import Prop._
import Arbitrary._

/** Class that represents a generator. */
class Gen[+T](g: Gen.Params => Option[T]) {

  var label = ""

  def label(l: String): this.type = {
    label = l
    this
  }

  def apply(prms: Gen.Params) = g(prms)

  def map[U](f: T => U): Gen[U] = new Gen(prms => this(prms).map(f)).label(label)

  def map2[U, V](g: Gen[U])(f: (T, U) => V) =
    combine(g)((t, u) => t.flatMap(t => u.flatMap(u => Some(f(t, u)))))

  def map3[U, V, W](gu: Gen[U], gv: Gen[V])(f: (T, U, V) => W) =
    combine3(gu, gv)((t, u, v) => t.flatMap(t => u.flatMap(u => v.flatMap(v => Some(f(t, u, v))))))

  def map4[U, V, W, X](gu: Gen[U], gv: Gen[V], gw: Gen[W])(f: (T, U, V, W) => X) =
    combine4(gu, gv, gw)((t, u, v, w) => t.flatMap(t => u.flatMap(u => v.flatMap(v => w.flatMap(w => Some(f(t, u, v, w)))))))

  def map5[U, V, W, X, Y](gu: Gen[U], gv: Gen[V], gw: Gen[W], gx: Gen[X])(f: (T, U, V, W, X) => Y) =
    combine5(gu, gv, gw, gx)((t, u, v, w, x) => t.flatMap(t => u.flatMap(u => v.flatMap(v => w.flatMap(w => x.flatMap(x => Some(f(t, u, v, w, x))))))))

  def map6[U, V, W, X, Y, Z](gu: Gen[U], gv: Gen[V], gw: Gen[W], gx: Gen[X], gy: Gen[Y])(f: (T, U, V, W, X, Y) => Z) =
    combine6(gu, gv, gw, gx, gy)((t, u, v, w, x, y) => t.flatMap(t => u.flatMap(u => v.flatMap(v => w.flatMap(w => x.flatMap(x => y.flatMap(y => Some(f(t, u, v, w, x, y)))))))))

  def flatMap[U](f: T => Gen[U]): Gen[U] = new Gen(prms => for {
    t <- this(prms)
    u <- f(t)(prms)
  } yield u)

  def filter(p: T => Boolean): Gen[T] = new Gen(prms => for {
    t <- this(prms)
    u <- if (p(t)) Some(t) else None
  } yield u).label(label)

  def suchThat(p: T => Boolean): Gen[T] = filter(p)

  def combine[U,V](g: Gen[U])(f: (Option[T],Option[U]) => Option[V]): Gen[V] =
    new Gen(prms => f(this(prms), g(prms)))

  def combine3[U, V, W](gu: Gen[U], gv: Gen[V])
      (f: (Option[T], Option[U], Option[V]) => Option[W]) =
    new Gen(prms => f(this(prms), gu(prms), gv(prms)))

  def combine4[U, V, W, X](gu: Gen[U], gv: Gen[V], gw: Gen[W])
      (f: (Option[T], Option[U], Option[V], Option[W]) => Option[X]) =
    new Gen(prms => f(this(prms), gu(prms), gv(prms), gw(prms)))

  def combine5[U, V, W, X, Y](gu: Gen[U], gv: Gen[V], gw: Gen[W], gx: Gen[X])
      (f: (Option[T], Option[U], Option[V], Option[W], Option[X]) => Option[Y]) =
    new Gen(prms => f(this(prms), gu(prms), gv(prms), gw(prms), gx(prms)))

  def combine6[U, V, W, X, Y, Z](gu: Gen[U], gv: Gen[V], gw: Gen[W], gx: Gen[X], gy: Gen[Y])
      (f: (Option[T], Option[U], Option[V], Option[W], Option[X], Option[Y]) => Option[Z]) =
        new Gen(prms => f(this(prms), gu(prms), gv(prms), gw(prms), gx(prms), gy(prms)))

  def ap[U](g: Gen[T => U]) = flatMap(t => g.flatMap(u => new Gen(p => Some(u(t)))))

  override def toString =
    if(label.length == 0) "Gen()" else "Gen(\"" + label + "\")"

  /** Returns a new property that holds if and only if both this
   *  and the given generator generates the same result. */
  def ==[U](g: Gen[U]) = forAll(this)(r => forAll(g)(_ == r))

  /** Returns a new property that holds if and only if both this
   *  and the given generator generates the same result, or both
   *  generators generate no result.  */
  def ===[U](g: Gen[U]) = new Prop(prms =>
    (this(prms), g(prms)) match {
      case (None,None) => proved(prms)
      case (Some(r1),Some(r2)) if r1 == r2 => proved(prms)
      case _ => falsified(prms)
    }
  )

  def !=[U](g: Gen[U]) = forAll(this)(r => forAll(g)(_ != r))

  def !==[U](g: Gen[U]) = new Prop(prms =>
    (this(prms), g(prms)) match {
      case (None,None) => falsified(prms)
      case (Some(r1),Some(r2)) if r1 == r2 => falsified(prms)
      case _ => proved(prms)
    }
  )

}


/** Contains combinators for building generators. */
object Gen extends Properties {

  val name = "Gen"

  /** Record that encapsulates all parameters required for data generation */
  case class Params(size: Int, rand: RandomGenerator) {
    def resize(newSize: Int) = Params(newSize,rand)
  }

  /* Default generator parameters */
  val defaultParams = Params(100,StdRand)


  // Generator combinators

  /* Sequences generators. If any of the given generators fails, the
   * resulting generator will also fail. */
  def sequence[T](gs: Iterable[Gen[T]]): Gen[Seq[T]] = new Gen(prms => {
    val buf = new ListBuffer[T]
    var none = false
    val xs = gs.map(g => g(prms)).elements
    while(xs.hasNext && !none) xs.next match {
      case None => none = true
      case Some(x) => buf += x
    }
    if(none) None else Some(buf)
  })
  specify("sequence",
    forAllDefaultShrink(listOf(elementsFreq((10,arbitrary[Int]),(1,fail))))(l =>
      (someFailing(l) && (sequence(l) === fail)) ||
      (noneFailing(l) && forAll(sequence(l)) { _.length == l.length })
    )
  )

  /** Wraps a generator lazily. Useful when defining recursive generators. */
  def lzy[T](g: => Gen[T]) = new Gen(p => g(p))
  specify("lzy", { g: Gen[Int] => lzy(g) === g })

  /** A generator that always generates the given value */
  def value[T](x: T) = new Gen(p => Some(x))
  specify("value", (x: Int, prms: Params) => value(x)(prms) == Some(x))

  /** A generator that always generates the given value */
  def value[T](f: () => T) = new Gen(p => Some(f()))
  specify("value", (x: Int) => value(() => x) === value(x))

  /** A generator that never generates a value */
  def fail[T]: Gen[T] = new Gen(p => None)
  specify("fail", (prms: Params) => fail(prms) == None)

  /** A generator that generates a random integer in the given (inclusive)
   *  range. */
  def choose(low: Int, high: Int) = if(low > high) fail else
    parameterized(prms => value(prms.rand.choose(low,high)))
  specify("choose-int", { (l: Int, h: Int) =>
    if(l > h) choose(l,h) === fail
    else forAll(choose(l,h)) { x => x >= l && x <= h }
  })

  /** A generator that generates a random double in the given (inclusive)
   *  range. */
  def choose(low: Double, high: Double) = if(low > high) fail else
    parameterized(prms => value(prms.rand.choose(low,high)))
  specify("choose-double", { (l: Double, h: Double) =>
    if(l > h) choose(l,h) === fail
    else forAll(choose(l,h)) { x => x >= l && x <= h }
  })

  /** Creates a generator that can access its generation parameters */
  def parameterized[T](f: Params => Gen[T]): Gen[T] =
    new Gen(prms => f(prms)(prms))
  specify("parameterized", (g: Gen[Int]) => parameterized(p => g) === g)

  /** Creates a generator that can access its generation size */
  def sized[T](f: Int => Gen[T]) = parameterized(prms => f(prms.size))
  specify("sized", (g: Gen[Int]) => sized(i => g) === g)

  /** Creates a resized version of a generator */
  def resize[T](s: Int, g: Gen[T]) = new Gen(prms => g(prms.resize(s)))

  /** Chooses one of the given generators with a weighted random distribution */
  def frequency[T](gs: (Int,Gen[T])*): Gen[T] = {
    lazy val tot = (gs.map(_._1) :\ 0) (_+_)

    def pick(n: Int, l: List[(Int,Gen[T])]): Gen[T] = l match {
      case Nil => fail
      case (k,g)::gs => if(n <= k) g else pick(n-k, gs)
    }

    for {
      n <- choose(1,tot)
      x <- pick(n,gs.toList)
    } yield x
  }

  /** Chooses one of the given values, with a weighted random distribution. */
  def elementsFreq[T](vs: (Int, T)*): Gen[T] =
    frequency(vs.map { case (w,v) => (w, value(v)) } : _*)
  specify("elements", { l: List[Int] =>
    if(l.isEmpty) elements(l: _*) == fail
    else forAll(elements(l: _*))(l.contains)
  })

  /** A generator that returns a random element from a list */
  def elements[T](xs: T*): Gen[T] = if(xs.isEmpty) fail else for {
    i <- choose(0,xs.length-1)
  } yield xs(i)

  /** Picks a random generator from a list */
  def oneOf[T](gs: Gen[T]*) = if(gs.isEmpty) fail else for {
    i <- choose(0,gs.length-1)
    x <- gs(i)
  } yield x

  /** Generates a list of random length. The maximum length depends on the
   *  size parameter */
  def listOf[T](g: => Gen[T]) = sized(size => for {
    n <- choose(0,size)
    l <- vectorOf(n,g)
  } yield l.toList)

  /** Generates a non-empty list of random length. The maximum length depends
   *  on the size parameter */
  def listOf1[T](g: => Gen[T]) = for {
    x  <- g
    xs <- listOf(g)
  } yield x::xs

  /** Generates a list of the given length */
  def vectorOf[T](n: Int, g: Gen[T]) = sequence(List.make(n,g))

  /** Picks some elements from a list */
  def someOf[T](l: Collection[T]) = choose(0,l.size) flatMap (pick(_,l))
  specify("someOf", { l: List[Int] =>
    forAllDefaultShrink(someOf(l).map(_.toList)) { _.forall(l.contains) }
  })

  /** Picks a given number of elements from a list */
  def pick[T](n: Int, l: Collection[T]): Gen[Seq[T]] =
    if(n > l.size || n < 0) fail
    else new Gen(prms => {
      val buf = new ListBuffer[T]
      buf ++= l
      while(buf.length > n) buf.remove(choose(0,buf.length-1)(prms).get)
      Some(buf)
    })
  specify("pick", { l: List[Int] =>
    forAll(choose(-1,2*l.length)) { n =>
      if(n < 0 || n > l.length) pick(n,l) === fail
      else forAll(pick(n,l)) { m => m.length == n && m.forall(l.contains) }
    }
  })

  /* Generates a numerical character */
  def numChar: Gen[Char] = choose(48,57) map (_.toChar)
  specify("numChar", forAll(numChar)(_.isDigit))

  /* Generates an upper-case alpha character */
  def alphaUpperChar: Gen[Char] = choose(65,90) map (_.toChar)
  specify("alphaUpperChar",
    forAll(alphaUpperChar)(c => c.isLetter && c.isUpperCase))

  /* Generates a lower-case alpha character */
  def alphaLowerChar: Gen[Char] = choose(97,122) map (_.toChar)
  specify("alphaLowerChar",
    forAll(alphaLowerChar)(c => c.isLetter && c.isLowerCase))

  /* Generates an alpha character */
  def alphaChar = frequency((1,alphaUpperChar), (9,alphaLowerChar))
  specify("alphaChar", forAll(alphaChar)(_.isLetter))

  /* Generates an alphanumerical character */
  def alphaNumChar = frequency((1,numChar), (9,alphaChar))
  specify("alphaNumChar", forAll(alphaNumChar)(_.isLetterOrDigit))

  /* Generates a string that starts with a lower-case alpha character,
   * and only contains alphanumerical characters */
  def identifier: Gen[String] = for {
    c <- alphaLowerChar
    cs <- listOf(alphaNumChar)
  } yield List.toString(c::cs)
  specify("identifier", forAll(identifier)(s =>
    s.length > 0 && s(0).isLetter && s(0).isLowerCase &&
    s.forall(_.isLetterOrDigit)
  ))

}
