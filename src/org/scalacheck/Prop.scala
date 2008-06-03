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

/** A property is a generator that generates a property result */
trait Prop extends Gen[(Prop.Result, Prop.CollectedData)] {

  import Prop.{Proof,True,False,Exception}

  /** Convenience method that makes it possible to use a this property
   *  as an application that checks itself on execution */
  def main(args: Array[String]) { check }

  /** Convenience method that checks this property and reports the
   *  result on the console. Calling <code>p.check</code> is equal
   *  to calling <code>Test.check(p)</code>, but this method does
   *  not return the test statistics. If you need to get the results
   *  from the test, or if you want more control over the test parameters,
   *  use the <code>check</code> methods in <code>Test</code> instead. */
  def check: Unit = Test.check(this)

  /** Returns a new property that holds if and only if both this
   *  and the given property hold. If one of the properties doesn't
   *  generate a result, the new property will generate false.
   */
  def &&(p: Prop): Prop = combine(p) {
    case (x@Some((Exception(_),_)), _) => x
    case (_, x@Some((Exception(_),_))) => x

    case (x, Some((True,_))) => x
    case (Some((True,_)), x) => x

    case (x, Some((Proof,_))) => x
    case (Some((Proof,_)), x) => x

    case (x@Some((False,_)), _) => x
    case (_, x@Some((False,_))) => x

    case _ => None
  }

  /** Returns a new property that holds if either this
   *  or the given property (or both) hold.
   */
  def ||(p: Prop): Prop = combine(p) {
    case (x@Some((Exception(_),_)), _) => x
    case (_, x@Some((Exception(_),_))) => x

    case (x@Some((Proof,_)), _) => x
    case (_, x@Some((Proof,_))) => x

    case (x@Some((True,_)), _) => x
    case (_, x@Some((True,_))) => x

    case (Some((False,_)), x) => x
    case (x, Some((False,_))) => x

    case _ => None
  }

  /** Returns a new property that holds if and only if both this
   *  and the given property hold. If one of the properties doesn't
   *  generate a result, the new property will generate the same result
   *  as the other property.
   */
  def ++(p: Prop): Prop = combine(p) {
    case (x@Some((Exception(_),_)), _) => x
    case (_, x@Some((Exception(_),_))) => x

    case (None, x) => x
    case (x, None) => x

    case (x, Some((Proof,_))) => x
    case (Some((Proof,_)), x) => x

    case (x, Some((True,_))) => x
    case (Some((True,_)), x) => x

    case (x@Some((False,_)), _) => x
    case (_, x@Some((False,_))) => x
  }

  /** Returns a new property that holds if and only if both this
   *  and the given property generates the same result, or both
   *  properties generate no result.
   */
  def ==(p: Prop): Prop = flatCombine(p) {
    case (r1,r2) => (r1.map(_._1), r2.map(_._1)) match {
      case (None,None) => Prop.proved
      case (Some(r1),Some(r2)) if r1 == r2 => Prop.proved
      case _ => Prop.falsified
    }
  }

  def addArg(arg: Prop.Arg): Prop = for((r,cd) <- this) yield (r, cd.addArg(arg))

  def collect(v: Any): Prop = for((r,cd) <- this) yield (r, cd.collect(v))

  override def toString =
    if(label.length == 0) "Prop()" else "Prop(\"" + label + "\")"

}

object Prop {

  /** Specifications for the methods in <code>Prop</code> */
  val specification = new Properties("Prop")

  import specification.specify
  import Gen.{value, fail, frequency, elements}
  import Arbitrary._
  import Shrink._


  // Specifications for the Prop class

  specify("Prop.&& Commutativity", (p1: Prop, p2: Prop) =>
    (p1 && p2) === (p2 && p1)
  )
  specify("Prop.&& Exception", (p: Prop) =>
    (p && exception(null)) == exception(null)
  )
  specify("Prop.&& True", (p: Prop) =>
    (p && passed) === p
  )
  specify("Prop.&& Proof", (p: Prop) =>
    (p && proved) === p
  )
  specify("Prop.&& False", {
    val g = elements(passed,proved,falsified,undecided)
    forAll(g)(p => (p && falsified) == falsified)
  })
  specify("Prop.&& Undecided", {
    val g = elements(proved,undecided)
    forAll(g)(p => (p && undecided) === undecided)
  })
  specify("Prop.&& Right prio", (sz: Int) => {
    val p = proved.addArg(Arg("","RHS",0)) && proved.addArg(Arg("","LHS",0))
    p(Gen.Params(sz,StdRand)) match {
      case Some(r) if r._2.args == Arg("","RHS",0)::Nil => true
      case _ => false
    }
  })

  specify("Prop.|| Commutativity", (p1: Prop, p2: Prop) =>
    (p1 || p2) === (p2 || p1)
  )
  specify("Prop.|| Exception", (p: Prop) =>
    (p || exception(null)) == exception(null)
  )
  specify("Prop.|| Identity", (p: Prop) =>
    (p || falsified) === p
  )
  specify("Prop.|| True", {
    val g = elements(passed,falsified,undecided)
    forAll(g)(p => (p || passed) == passed)
  })
  specify("Prop.|| Proof", {
    val g = elements(passed,proved,falsified,undecided)
    forAll(g)(p => (p || proved) == proved)
  })
  specify("Prop.|| Undecided", {
    val g = elements(falsified,undecided)
    forAll(g)(p => (p || undecided) === undecided)
  })

  specify("Prop.++ Commutativity", (p1: Prop, p2: Prop) =>
    (p1 ++ p2) === (p2 ++ p1)
  )
  specify("Prop.++ Exception", (p: Prop) =>
    (p ++ exception(null)) == exception(null)
  )
  specify("Prop.++ Identity 1", {
    val g = elements(passed,falsified,proved,exception(null))
    forAll(g)(p => (p ++ proved) === p)
  })
  specify("Prop.++ Identity 2", (p: Prop) =>
    (p ++ undecided) === p
  )
  specify("Prop.++ False", {
    val g = elements(passed,falsified,proved,undecided)
    forAll(g)(p => (p ++ falsified) === falsified)
  })


  // Types

  sealed case class Arg(label: String, arg: Any, shrinks: Int)

  type Args = List[Arg]

  case class CollectedData(args: Args, freqMap: Map[Any,Int]) {
    def collect(value: Any): CollectedData = freqMap.get(value) match {
      case None => CollectedData(args, freqMap + (value -> 1))
      case Some(n) => CollectedData(args, freqMap + (value -> (n+1)))
    }

    def collect[T,U](f: T => U)(value: T): CollectedData = collect(f(value))

    def addArg(arg: Arg) = CollectedData(arg::args, freqMap)
  }

  case object NoCollectedData extends CollectedData(Nil, Map.empty)

  /** The result of evaluating a property */
  sealed trait Result {
    def success = this match {
      case True => true
      case Proof => true
      case _ => false
    }

    def failure = this match {
      case False => true
      case Exception(_) => true
      case _ => false
    }
  }

  case object True extends Result
  case object Proof extends Result
  case object False extends Result
  case class Exception(e: Throwable) extends Result {
    override def equals(x: Any) = x match {
      case Exception(_) => true
      case _ => false
    }
  }


  // Implicit defs

  implicit def extendedBoolean(b: Boolean) = new {
    /** Implication */
    def ==>(p: => Prop) = Prop.==>(b,p)
  }

  implicit def extendedAny[T](x: T) = new {
    def imply(f: PartialFunction[T,Prop]) = Prop.imply(x,f)
    def iff(f: PartialFunction[T,Prop]) = Prop.iff(x,f)
  }

  implicit def propBoolean(b: Boolean): Prop = if(b) proved else falsified


  // Private support methods

  private def constantProp(r: Result): Prop = Gen.value((r,NoCollectedData))

  private implicit def genToProp(g: Gen[(Result,CollectedData)]) = Prop(g.apply).label(g.label)
//  private implicit def genToProp(g: Gen[Result]) = new Prop {
//    def apply(p: Gen.Params) = g(p).map((_,NoCollectedData))
//    label(g.label)
//  }

  private def provedToTrue(r: Result) = r match {
    case Proof => True
    case _ => r
  }


  // Public support  methods

  /** Property factory method */
  def apply(g: Gen.Params => Option[(Result,CollectedData)]) = new Prop {
    def apply(p: Gen.Params) = g(p)
  }


  // Property combinators

  /** A property that never is proved or falsified */
  lazy val undecided: Prop = Gen.fail.label("undecided")
  specify("undecided", (prms: Gen.Params) => undecided(prms) == None)

  /** A property that always is false */
  lazy val falsified = constantProp(False).label("failed")
  specify("falsified", (prms: Gen.Params) => falsified(prms) iff {
    case Some((False,_)) => true
  })

  /** A property that always is proved */
  lazy val proved = constantProp(Proof).label("proved")
  specify("proved", (prms: Gen.Params) => proved(prms) iff {
    case Some((Proof,_)) => true
  })

  /** A property that always is passed */
  lazy val passed = constantProp(True).label("passed")
  specify("passed", (prms: Gen.Params) => passed(prms) iff {
    case Some((True,_)) => true
  })

  /** A property that denotes an exception */
  def exception(e: Throwable) = constantProp(Exception(e)).label("exception")
  specify("exception",(prms:Gen.Params, e:Throwable) => exception(e)(prms) iff {
    case Some((Exception(f),_)) if f == e => true
  })

  /** A property that depends on the generator size */
  def sizedProp(f: Int => Prop): Prop = Prop(prms => f(prms.size)(prms))

  /** Implication */
  def ==>(b: => Boolean, p: => Prop): Prop = property(if (b) p else undecided)

  /** Implication with several conditions */
  def imply[T](x: T, f: PartialFunction[T,Prop]): Prop =
    property(if(f.isDefinedAt(x)) f(x) else undecided)

  /** Property holds only if the given partial function is defined at
   *  <code>x</code>, and returns a property that holds */
  def iff[T](x: T, f: PartialFunction[T,Prop]): Prop =
    property(if(f.isDefinedAt(x)) f(x) else falsified)

  /** Combines properties into one, which is true if and only if all the
   *  properties are true */
  def all(ps: Iterable[Prop]) = Prop { prms =>
    val it = ps.elements
    var res = proved(prms)
    while(res.map(_._1.success).getOrElse(false) && it.hasNext)
      res = (Prop(prms => res) && it.next)(prms)
    res
  }
  specify("all", forAll(Gen.listOf1(value(proved)))(l => all(l)))

  /** Combines properties into one, which is true if at least one of the
   *  properties is true */
  def atLeastOne(ps: Iterable[Prop]) = Prop { prms =>
    val it = ps.elements
    var res = falsified(prms)
    while(!res.map(_._1.success).getOrElse(false) && it.hasNext)
      res = (Prop(prms => res) || it.next)(prms)
    res
  }
  specify("atLeastOne", forAll(Gen.listOf1(value(proved)))(l => atLeastOne(l)))

  /** Existential quantifier */
  def exists[A,P <% Prop](g: Gen[A])(f: A => P): Prop = for {
    a <- g
    (r,cd) <- property(f(a))
    (s,_) <- r match {
      case True => proved
      case Proof => proved
      case False => undecided
      case Exception(e) => exception(e)
    }
  } yield (s, cd.addArg(Arg(g.label,a,0)))

  /** Universal quantifier, does not shrink failed test cases. */
  def forAllNoShrink[A,P <% Prop](g: Gen[A])(f: A => P): Prop = for {
    a <- g
    (r,cd) <- property(f(a))
  } yield (provedToTrue(r), cd.addArg(Arg(g.label,a,0)))

  /** Universal quantifier, shrinks failed arguments with given shrink
   *  function */
  def forAllShrink[A, P <% Prop](g: Gen[A],shrink: A => Stream[A])(f: A => P): Prop =
    Prop((prms: Gen.Params) => {

      import Stream.{cons, empty}

      def getFirstFail(xs: Stream[A], shrinks: Int) = {
        val results = xs.map { x =>
          val p = property(f(x))
          p(prms).map { case (r,cd) => (x, (provedToTrue(r), cd.addArg(Arg(g.label,x,shrinks)))) }
        }
        results match {
          case Stream.empty => None
          case _ => results.dropWhile(!isFailure(_)) match {
            case Stream.empty => results.head
            case failures => failures.head
          }
        }
      }

      def isFailure(r: Option[(A,(Result,CollectedData))]) = r match {
        case Some((_,res)) => res._1.failure
        case _ => false
      }

      g(prms) match {
        case None => None
        case Some(x) =>
          var shrinks = 0
          var xr = getFirstFail(cons(x, empty), shrinks)
          if(!isFailure(xr)) xr.map(_._2)
          else {
            var r: Option[(Result,CollectedData)] = None
            do {
              shrinks += 1
              r = xr.map(_._2)
              xr = getFirstFail(shrink(xr.get._1), shrinks)
            } while(isFailure(xr))
            r
          }
      }
    })

  /** Universal quantifier, shrinks failed arguments with the default
   *  shrink function for the type */
  def forAll[T,P](g: Gen[T])(f: T => P)
    (implicit s: Shrink[T], p: P => Prop) = forAllShrink(g, shrink[T])(f)

  def collect[T,P <% Prop](f: T => P): T => Prop = t => f(t).collect(t)

  /** A property that holds if at least one of the given generators
   *  fails generating a value */
  def someFailing[T](gs: Iterable[Gen[T]]) = atLeastOne(gs.map(_ === fail))

  /** A property that holds iff none of the given generators
   *  fails generating a value */
  def noneFailing[T](gs: Iterable[Gen[T]]) = all(gs.map(_ !== fail))

  /** Wraps and protects a property */
  def property[P <% Prop](p: => P): Prop = Prop(prms =>
    (try { p: Prop } catch { case e => exception(e) })(prms)
  )

  /** Converts a function into a property */
  def property[A1,P] (
    f:  A1 => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1]
  ) = forAllShrink(arbitrary[A1],shrink[A1])(f andThen p)

  /** Converts a function into a property */
  def property[A1,A2,P] (
    f:  (A1,A2) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2]
  ): Prop = property((a: A1) => property(f(a, _:A2)))

  /** Converts a function into a property */
  def property[A1,A2,A3,P] (
    f:  (A1,A2,A3) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3]
  ): Prop = property((a: A1) => property(f(a, _:A2, _:A3)))

  /** Converts a function into a property */
  def property[A1,A2,A3,A4,P] (
    f:  (A1,A2,A3,A4) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4]
  ): Prop = property((a: A1) => property(f(a, _:A2, _:A3, _:A4)))

  /** Converts a function into a property */
  def property[A1,A2,A3,A4,A5,P] (
    f:  (A1,A2,A3,A4,A5) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5]
  ): Prop = property((a: A1) => property(f(a, _:A2, _:A3, _:A4, _:A5)))

  /** Converts a function into a property */
  def property[A1,A2,A3,A4,A5,A6,P] (
    f:  (A1,A2,A3,A4,A5,A6) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5],
    a6: Arbitrary[A6], s6: Shrink[A6]
  ): Prop = property((a: A1) => property(f(a, _:A2, _:A3, _:A4, _:A5, _:A6)))

}
