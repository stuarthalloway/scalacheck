package scalacheck

trait Testable {

  import scala.collection.Map
  import scala.testing.SUnit.TestCase

  private var properties = scala.collection.immutable.Map.empty[String, Prop]

  // Implicit defs

  implicit def extendedBoolean(b: Boolean) = new Prop.ExtendedBoolean(b)
  implicit def extendedAny[T](x: T) = new Prop.ExtendedAny(x)

  protected def addProperty[P]
    (propName: String, f: () => P)(implicit
     p:  P => Prop): Unit =
  {
    properties = properties.update(propName,Prop.property(f))
  }

  protected def addProperty[A1,P]
    (propName: String, f: A1 => P)(implicit
     p:  P => Prop,
     g1: Arbitrary[A1] => Gen[A1]): Unit =
  {
    properties = properties.update(propName,Prop.property(f))
  }

  protected def addProperty[A1,A2,P]
    (propName: String, f: (A1,A2) => P)(implicit
     p:  P => Prop,
     g1: Arbitrary[A1] => Gen[A1],
     g2: Arbitrary[A2] => Gen[A2]): Unit =
  {
    properties = properties.update(propName,Prop.property(f))
  }

  protected def addProperty[A1,A2,A3,P]
    (propName: String, f: (A1,A2,A3) => P)(implicit
     p:  P => Prop,
     g1: Arbitrary[A1] => Gen[A1],
     g2: Arbitrary[A2] => Gen[A2],
     g3: Arbitrary[A3] => Gen[A3]): Unit =
  {
    properties = properties.update(propName,Prop.property(f))
  }

  protected def addProperty[A1,A2,A3,A4,P]
    (propName: String, f: (A1,A2,A3,A4) => P)(implicit
     p:  P => Prop,
     g1: Arbitrary[A1] => Gen[A1],
     g2: Arbitrary[A2] => Gen[A2],
     g3: Arbitrary[A2] => Gen[A3],
     g4: Arbitrary[A3] => Gen[A4]): Unit =
  {
    properties = properties.update(propName,Prop.property(f))
  }

  protected def addProperty(propName: String, prop: Prop): Unit =
    properties = properties.update(propName, prop)

  type TestsInspector = (String,Option[Prop.Result],Int,Int) => Unit
  type TestStatsInspector = (String,Test.Stats) => Unit

  /** Tests all properties with the given testing parameters, and returns
   *  the test results.
   */
  def checkProperties(prms: Test.Params): Map[String,Test.Stats] =
    checkProperties(prms, (n,r,s,d) => (), (n,s) => ())

  /** Tests all properties with the given testing parameters, and returns
   *  the test results. <code>f</code> is a function which is called each
   *  time a property is evaluted. <code>g</code> is a function called each
   *  time a property has been fully tested.
   */
  def checkProperties(prms: Test.Params, f: TestsInspector, g: TestStatsInspector
  ): Map[String,Test.Stats] = properties transform { case (pName,p) =>
    val stats = Test.check(prms,p,f(pName,_,_,_))
    g(pName,stats)
    stats
  }

  /** Tests all properties with default testing parameters, and returns
   *  the test results. The results are also printed on the console during
   *  testing.
   */
  def checkProperties(): Map[String,Test.Stats] =
  {
    def printTmp(pn: String, res: Option[Prop.Result], succ: Int, disc: Int) = {
      if(disc > 0)
        Console.printf("\r{3}: Passed {0} tests; {1} discarded",succ,disc,pn)
      else
        Console.printf("\r{1}: Passed {0} tests",succ,pn)
      Console.flush
    }

    def printStats(pName: String, stats: Test.Stats) = stats.result match {
      case Test.GenException(e) =>
        Console.printf("\r{1}: *** Exception raised when generating arguments:\n{0}               \n\n",
          e, pName)
      case Test.PropException(e,args) =>
        Console.printf("\r{0}: *** Exception raised when evaluating property                        \n",
          pName)
        Console.printf("The arguments that caused the exception was:\n{0}\n\n", args)
        Console.printf("The raised exception was:\n{0}\n\n", e)
      case Test.Failed(args) =>
        Console.printf("\r{1}: *** Failed after {0} successful tests                                \n",
          stats.succeeded, pName)
        Console.printf("The arguments that caused the failure was:\n{0}\n\n", args)
      case Test.Exhausted() =>
        Console.printf("\r{2}: *** Gave up, after only {1} passed tests. {0} tests were discarded.\n\n",
          stats.discarded, stats.succeeded, pName)
      case Test.Passed() =>
        Console.printf("\r{1}: +++ OK, passed {0} tests.                                          \n\n",
          stats.succeeded, pName)
    }

    checkProperties(Test.defaultParams,printTmp,printStats)
  }

  private def propToTestCase(pn: String, p: Prop): TestCase = new TestCase(pn) {

    protected def runTest() = {
      val stats = Test.check(Test.defaultParams,p)
      stats.result match {
        case Test.GenException(e) => fail(
          " Exception raised when generating arguments.\n" +
          "The raised exception was:\n"+e.toString+"\n")
        case Test.PropException(e,args) => fail(
          " Exception raised when evaluating property.\n\n" +
          "The arguments that caused the failure was:\n"+args.toString+"\n\n" +
          "The raised exception was:\n"+e.toString+"\n")
        case Test.Failed(args) => fail(
          " Property failed after " + stats.succeeded.toString +
          " successful tests.\n" +
          "The arguments that caused the failure was:\n"+args.toString+"\n\n")
        case Test.Exhausted() => fail(
          " Gave up after only " + stats.succeeded.toString + " tests. " +
          stats.discarded.toString + " tests were discarded.")
        case Test.Passed() => ()
      }
    }

  }

  /** Returns all properties as SUnit.TestCase instances, which can added to
   *  a SUnit.TestSuite.
   */
  def testCases: List[TestCase] =
    (properties map {case (pn,p) => propToTestCase(pn,p)}).toList

}
