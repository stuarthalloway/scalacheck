package scalacheck

import scala.collection.mutable.ListBuffer

/** Class that represents a generator. */
class Gen[+T](g: Gen.Params => Option[T]) {

  def apply(prms: Gen.Params) = g(prms)

  def map[U](f: T => U): Gen[U] = new Gen(prms => this(prms).map(f))
  
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
  } yield u)

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
}



/** Contains combinators for building generators. */
object Gen extends Properties {

  import Prop._

  // Types

  /** Record that encapsulates all parameters required for data generation */
  case class Params(size: Int, rand: RandomGenerator) {
    def resize(newSize: Int) = Params(newSize,rand)
  }

  val defaultParams = Params(100,StdRand)


  // Generator combinators

  /** Wraps a generator lazily. Useful when defining recursive generators. */
  def lzy[T](g: => Gen[T]) = new Gen(p => g(p))

  specify("Gen.value", (x: Int, prms: Params) =>
    value(x)(prms).get == x
  )

  /** A generator that always generates a given value */
  def value[T](x: T) = new Gen(p => Some(x))


  specify("Gen.fail", (x: Int, prms: Params) =>
    fail(prms) == None
  )

  /** A generator that never generates a value */
  def fail[T]: Gen[T] = new Gen(p => None)


  specify("Gen.choose-int", (l: Int, h: Int, prms: Params) => {
    val x = choose(l,h)(prms).get
    h >= l ==> (x >= l && x <= h)
  })

  /** A generator that generates a random integer in the given (inclusive)
   *  range.  */
  def choose(low: Int, high: Int) =
    parameterized(prms => value(prms.rand.choose(low,high)))


  specify("Gen.choose-double", (l: Double, h: Double, prms: Params) => {
    val x = choose(l,h)(prms).get
    h >= l ==> (x >= l && x <= h)
  })

  /** A generator that generates a random integer in the given (inclusive)
   *  range.  */
  def choose(low: Double, high: Double) =
    parameterized(prms => value(prms.rand.choose(low,high)))


  /** Creates a generator that can access its generation parameters
   */
  def parameterized[T](f: Params => Gen[T]): Gen[T] =
    new Gen(prms => f(prms)(prms))


  /** Creates a generator that can access its generation size
   */
  def sized[T](f: Int => Gen[T]) = parameterized(prms => f(prms.size))


  /** Creates a resized version of a generator
   */
  def resize[T](s: Int, g: Gen[T]) = new Gen(prms => g(prms.resize(s)))


  /** Chooses one of the given generators, with a weighted random distribution.
   */
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


  /** Chooses one of the given values, with a weighted random distribution.  */
  def elementsFreq[T](vs: (Int, T)*): Gen[T] = 
    frequency(vs.map { case (w,v) => (w, value(v)) } : _*)


  specify("Gen.elements", (l: List[Int], prms: Params) =>
    elements(l: _*)(prms) match {
      case None => l.isEmpty
      case Some(n) => l.contains(n)
    }
  )

  /** A generator that returns a random element from a list
   */
  def elements[T](xs: T*): Gen[T] = if(xs.isEmpty) fail else for {
    i <- choose(0,xs.length-1)
  } yield xs(i)


  /** Picks a random generator from a list */
  def oneOf[T](gs: Gen[T]*) = if(gs.isEmpty) fail else for {
    i <- choose(0,gs.length-1)
    x <- gs(i)
  } yield x


  /** Generates a list of random length. The maximum length depends on the
   *  size parameter
   */
  def listOf[T](g: => Gen[T]) = sized(size => for {
    n <- choose(0,size)
    l <- vectorOf(n,g)
  } yield l.toList)


  /** Generates a non-empty list of random length. The maximum length depends
   *  on the size parameter
   */
  def listOf1[T](g: Gen[T]) = for {
    x  <- g
    xs <- listOf(g)
  } yield x::xs


  specify("Gen.vectorOf", (len: Int, prms: Params) =>
    () imply {
      case () if len == 0 =>
        vectorOf(len,fail)(prms).get.length == 0 &&
        vectorOf(len,value(0))(prms).get.length == 0
      case () if len > 0 =>
        vectorOf(len,fail)(prms) == None &&
        vectorOf(len,value(0))(prms).get.length == len
    }
  )

  /** Generates a list of the given length */
  def vectorOf[T](n: Int, g: Gen[T]): Gen[Seq[T]] = new Gen(prms => {
    val l = new ListBuffer[T]
    var i = 0
    var break = false
    while(!break && i < n) g(prms) match {
      case Some(x) =>
        l += x
        i += 1
      case None => break = true
    }
    if(break) None
    else Some(l)
  })

  /* Generates a numerical character */
  def numChar: Gen[Char] = choose(48,57) map (_.toChar)

  /* Generates an upper-case alpha character */
  def alphaUpperChar: Gen[Char] = choose(65,90) map (_.toChar)

  /* Generates a lower-case alpha character */
  def alphaLowerChar: Gen[Char] = choose(97,122) map (_.toChar)

  /* Generates an alpha character */
  def alphaChar = frequency((1,alphaUpperChar), (9,alphaLowerChar))

  /* Generates an alphanumerical character */
  def alphaNumChar = frequency((1,numChar), (9,alphaChar))

  /* Generates a string that starts with a lower-case alpha character, 
   * and only contains alphanumerical characters */
  def identifier: Gen[String] = for {
    c <- alphaLowerChar
    cs <- listOf(alphaNumChar)
  } yield List.toString(c::cs)

}
