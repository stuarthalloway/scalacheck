/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2008 Rickard Nilsson. All rights reserved.          **
**  http://code.google.com/p/scalacheck/                                   **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck

object Test {

  import ConsoleReporter.{testReport, propReport}

  private def secure[T](x: => T): Either[T,Throwable] =
    try { Left(x) } catch { case e => Right(e) }

  // Types

  /** Test parameters */
  case class Params(minSuccessfulTests: Int, maxDiscardedTests: Int,
    minSize: Int, maxSize: Int, rand: RandomGenerator)

  /** Test statistics */
  case class Result(status: Status, succeeded: Int, discarded: Int, freqMap: FreqMap[Any]) {
    def passed = status match {
      case Passed => true
      case Proved(_) => true
      case _ => false
    }
  }

  /** Test result */
  sealed trait Status

  /** ScalaCheck found enough cases for which the property holds, so the
   *  property is considered correct. (It is not proved correct, though). */
  case object Passed extends Status

  /** ScalaCheck managed to prove the property correct */
  sealed case class Proved(args: List[Arg]) extends Status 

  /** The property was proved wrong with the given concrete arguments.  */
  sealed case class Failed(args: List[Arg]) extends Status

  /** The property test was exhausted, it wasn't possible to generate enough
   *  concrete arguments satisfying the preconditions to get enough passing
   *  property evaluations. */
  case object Exhausted extends Status

  /** An exception was raised when trying to evaluate the property with the
   *  given concrete arguments. */
  sealed case class PropException(args: List[Arg], e: Throwable) extends Status

  /** An exception was raised when trying to generate concrete arguments
   *  for evaluating the property. */
  sealed case class GenException(e: Throwable) extends Status

  /** Property evaluation callback. Takes number of passed and
   *  discarded tests, respectively */
  type PropEvalCallback = (Int,Int) => Unit

  /** Property evaluation callback. Takes property name, and number of passed
   *  and discarded tests, respectively */
  type NamedPropEvalCallback = (String,Int,Int) => Unit

  /** Test callback. Takes property name, and test results. */
  type TestStatsCallback = (String,Result) => Unit

  /** Default testing parameters */
  val defaultParams = Params(100,500,0,100,StdRand)


  // Testing functions

  /** Tests a property with the given testing parameters, and returns
   *  the test results. */
  def check(prms: Params, p: Prop): Result = check(prms,p, (s,d) => ())

  /** Tests a property with the given testing parameters, and returns
   *  the test results. <code>propCallback</code> is a function which is
   *  called each time the property is evaluted. */
  def check(prms: Params, p: Prop, propCallback: PropEvalCallback): Result =
  {
    def result(s: Int, d: Int, sz: Float, freqMap: FreqMap[Any]): Result = {

      val size: Float = if(s == 0 && d == 0) prms.minSize else
        sz + ((prms.maxSize-sz)/(prms.minSuccessfulTests-s))

      val propPrms = Prop.Params(Gen.Params(size.round, prms.rand), freqMap)

      secure(p(propPrms)) match {
        case Right(e) => Result(GenException(e), s, d, FreqMap.empty[Any])
        case Left(propRes) => propRes.status match {
          case Prop.Undecided =>
            if(d+1 >= prms.maxDiscardedTests) Result(Exhausted, s, d+1, propRes.freqMap)
            else { propCallback(s, d+1); result(s, d+1, size, propRes.freqMap) }
          case Prop.True =>
            if(s+1 >= prms.minSuccessfulTests) Result(Passed, s+1, d, propRes.freqMap)
            else { propCallback(s+1, d); result(s+1, d,size, propRes.freqMap) }
          case Prop.Proof => Result(Proved(propRes.args), s+1, d, propRes.freqMap)
          case Prop.False => Result(Failed(propRes.args), s, d, propRes.freqMap)
          case Prop.Exception(e) => Result(PropException(propRes.args, e), s, d, propRes.freqMap)
        }
      }
    }

    result(0, 0, prms.minSize, FreqMap.empty[Any])
  }

  /** Tests a property with the given testing parameters, and returns
   *  the test results. <code>propCallback</code> is a function which is
   *  called each time the property is evaluted. Uses actors for parallel
   *  test execution, unless <code>workers</code> is less than or equal to 1.
   *  <code>worker</code> specifies how many working actors should be used.
   *  <code>wrkSize</code> specifies how many tests each worker should
   *  be scheduled with. */
/*  def check(prms: Params, p: Prop, propCallback: PropEvalCallback,
    workers: Int, wrkSize: Int
  ): Result =
    if(workers <= 1) check(prms,p,propCallback)
    else {
      assert(!p.isInstanceOf[Commands], "Commands cannot be checked multi-threaded")
      import scala.actors._
      import Actor._

      case class S(res: Result, s: Int, d: Int)

      val server = actor {
        var s = 0
        var d = 0
        var size: Float = prms.minSize
        var w = workers
        var stats: Stats = null
        loop { react {
          case 'wrkstop => w -= 1
          case 'get if w == 0 =>
            reply(stats)
            exit()
          case 'params => if(stats != null) reply() else {
            reply((s,d,size))
            size += wrkSize*((prms.maxSize-size)/(prms.minSuccessfulTests-s))
          }
          case S(res, sDelta, dDelta) if stats == null =>
            s += sDelta
            d += dDelta
            if(res != null) stats = Stats(res,s,d)
            else {
              if(s >= prms.minSuccessfulTests) stats = Stats(Passed,s,d)
              else if(d >= prms.maxDiscardedTests) stats = Stats(Exhausted,s,d)
              else propCallback(s,d)
            }
        }}
      }

      def worker = actor {
        var stop = false
        while(!stop) (server !? 'params) match {
          case (s: Int, d: Int, sz: Float) =>
            var s2 = s
            var d2 = d
            var size = sz
            var i = 0
            var res: Result = null
            while(res == null && i < wrkSize) {
              secure(p(Gen.Params(size.round, prms.rand))) match {
                case Left(propRes) => propRes match {
                  case None =>
                    d2 += 1
                    if(d2 >= prms.maxDiscardedTests) res = Exhausted
                  case Some(Prop.Proof(as)) =>
                    s2 += 1
                    res = Proved(as)
                  case Some(_: Prop.True) =>
                    s2 += 1
                    if(s2 >= prms.minSuccessfulTests) res = Passed
                  case Some(Prop.False(as)) => res = Failed(as)
                  case Some(Prop.Exception(as,e)) => res = PropException(as,e)
                }
                case Right(e) => res = GenException(e)
              }
              size += ((prms.maxSize-size)/(prms.minSuccessfulTests-s2))
              i += 1
            }
            server ! S(res,s2-s,d2-d)
          case _ => stop = true
        }
        server ! 'wrkstop
      }

      for(_ <- 1 to workers) worker
      (server !? 'get).asInstanceOf[Stats]
    }
*/

  /** Tests a property and prints results to the console. The 
   *  <code>maxDiscarded</code> parameter specifies how many 
   *  discarded tests that should be allowed before ScalaCheck
   *  gives up. */
  def check(p: Prop, maxDiscarded: Int): Result = {
    val Params(minSuccessfulTests, _, minSize, maxSize, rand) = defaultParams
    val params = Params(minSuccessfulTests,maxDiscarded,minSize,maxSize,rand)
    testReport(check(params, p, propReport))
  }

  /** Tests a property and prints results to the console */
  def check(p: Prop): Result = testReport(check(defaultParams, p, propReport))

  /** Tests all properties with the given testing parameters, and returns
   *  the test results. */
  def checkProperties(ps: Properties, prms: Params): Seq[(String,Result)] =
    checkProperties(ps, prms, (n,s,d) => (), (n,s) => ())

  /** Tests all properties with the given testing parameters, and returns
   *  the test results. <code>f</code> is a function which is called each
   *  time a property is evaluted. <code>g</code> is a function called each
   *  time a property has been fully tested. */
  def checkProperties(ps: Properties, prms: Params, 
    propCallback: NamedPropEvalCallback, testCallback: TestStatsCallback
  ): Seq[(String,Result)] = ps.properties.map { case (pName,p) =>
    val stats = check(prms,p,propCallback(pName,_,_))
    testCallback(pName,stats)
    (pName,stats)
  }

  /** Tests all properties with the given testing parameters, and returns
   *  the test results. <code>f</code> is a function which is called each
   *  time a property is evaluted. <code>g</code> is a function called each
   *  time a property has been fully testedi. Uses actors for execution. */
/*  def checkProperties(ps: Properties, prms: Params, 
    propCallback: NamedPropEvalCallback, testCallback: TestStatsCallback, 
    workers: Int, wrkSize: Int
  ): Seq[(String,Result)] = ps.properties.map { case (pName,p) =>
    val stats = check(prms,p,propCallback(pName,_,_),workers,wrkSize)
    testCallback(pName,stats)
    (pName,stats)
  }
*/
  /** Tests all properties with default testing parameters, and returns
   *  the test results. The results are also printed on the console during
   *  testing. */
  def checkProperties(ps: Properties): Seq[(String,Result)] = 
    checkProperties(ps, defaultParams, propReport, testReport)

}
