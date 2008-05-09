/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2008 Rickard Nilsson. All rights reserved.          **
**  http://code.google.com/p/scalacheck/                                   **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck

/** Represents a collection of properties, with convenient methods
 *  for checking all properties at once. Properties are added to this
 *  collection through the <code>specify</code> methods. This class
 *  is itself a property, that holds if and only if all of the
 *  contained properties hold. */
class Properties(val name: String) extends Prop {

  import scala.collection._
  import scala.testing.SUnit.TestCase
  import Arbitrary._
  import Shrink._

  type NamedPropEvalCallback = (String,Int,Int) => Unit
  type TestStatsCallback = (String,Test.Stats) => Unit

  private val properties = new mutable.ListBuffer[(String,Prop)]

  private def addProp(propName: String, prop: Prop) =
    properties += ((name+"."+propName, prop))

  private def allProperties: Prop = Prop.all((properties map (_._2)).toList)

  def apply(p: Gen.Params) = allProperties(p)

  /** Convenience method that makes it possible to use a this property
   *  collection as an application that checks all properties on
   *  execution */
  override def main(args: Array[String]) {
    checkProperties()
  }

  /** Adds all properties from another property collection to this one. */
  def include(props: Properties) = 
    props.properties.map { case (n,p) => addProp(n,p) }

  /** Adds a property to this property collection */
  def specify(propName: String, prop: => Prop) =
    addProp(propName, Prop.property(prop))

  /** Adds a property to this property collection */
  def specify[A1,P] (
    propName: String, f: A1 => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1]
  ): Unit = addProp(propName,Prop.property(f))

  /** Adds a property to this property collection */
  def specify[A1,A2,P] (
    propName: String, f: (A1,A2) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2]
  ): Unit = addProp(propName,Prop.property(f))

  /** Adds a property to this property collection */
  def specify[A1,A2,A3,P] (
    propName: String, f: (A1,A2,A3) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3]
  ): Unit = addProp(propName,Prop.property(f))

  /** Adds a property to this property collection */
  def specify[A1,A2,A3,A4,P] (
    propName: String, f: (A1,A2,A3,A4) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4]
  ): Unit = addProp(propName,Prop.property(f))

  /** Adds a property to this property collection */
  def specify[A1,A2,A3,A4,A5,P] (
    propName: String, f: (A1,A2,A3,A4,A5) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5]
  ): Unit = addProp(propName,Prop.property(f))

  /** Adds a property to this property collection */
  def specify[A1,A2,A3,A4,A5,A6,P] (
    propName: String, f: (A1,A2,A3,A4,A5,A6) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5],
    a6: Arbitrary[A6], s6: Shrink[A6]
  ): Unit = addProp(propName,Prop.property(f))

  /** Tests all properties with the given testing parameters, and returns
   *  the test results. */
  def checkProperties(prms: Test.Params): Seq[(String,Test.Stats)] =
    checkProperties(prms, (n,s,d) => (), (n,s) => ())

  /** Tests all properties with the given testing parameters, and returns
   *  the test results. <code>f</code> is a function which is called each
   *  time a property is evaluted. <code>g</code> is a function called each
   *  time a property has been fully tested. */
  def checkProperties(prms: Test.Params, propCallback: NamedPropEvalCallback,
    testCallback: TestStatsCallback
  ): Seq[(String,Test.Stats)] = properties.map { case (pName,p) =>
    val stats = Test.check(prms,p,propCallback(pName,_,_))
    testCallback(pName,stats)
    (pName,stats)
  }

  /** Tests all properties with the given testing parameters, and returns
   *  the test results. <code>f</code> is a function which is called each
   *  time a property is evaluted. <code>g</code> is a function called each
   *  time a property has been fully testedi. Uses actors for execution. */
  def checkProperties(prms: Test.Params, propCallback: NamedPropEvalCallback,
    testCallback: TestStatsCallback, workers: Int, wrkSize: Int
  ): Seq[(String,Test.Stats)] = properties.map { case (pName,p) =>
    val stats = Test.check(prms,p,propCallback(pName,_,_),workers,wrkSize)
    testCallback(pName,stats)
    (pName,stats)
  }

  /** Tests all properties with default testing parameters, and returns
   *  the test results. The results are also printed on the console during
   *  testing. */
  def checkProperties(): Seq[(String,Test.Stats)] = checkProperties(
    Test.defaultParams, 
    ConsoleReporter.propReport, 
    ConsoleReporter.testReport
  )

}
