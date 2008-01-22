/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2008 Rickard Nilsson. All rights reserved.          **
**  http://code.google.com/p/scalacheck/                                   **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck

/** Most of the arbitrary generators and shrinkers defined in this file
 *  are straightforward adaptations of the ones in Arbitrary.hs, from
 *  QuickCheck 2.
 */


/** The Arbitrary[T] class represents a type T that can be instantiated
 *  arbitrarily.
 *  To make your own instance of the Arbitrary class for a type T, define an
 *  implicit function that returns an instance of Arbitrary[T]. Use the factory
 *  method (apply) of the Arbitrary object to create the instance.
 */
sealed abstract class Arbitrary[T] {
  def arbitrary: Gen[T]
}


/** Contains Arbitrary instances for common types. */
object Arbitrary {

  import Gen.{value, choose, sized, elements, listOf, listOf1,
    frequency, oneOf, elementsFreq}

  /** Creates an instance of the Arbitrary class */
  def apply[T](g: Gen[T]) = new Arbitrary[T] {
    def arbitrary = g
  }

  /** Arbitrary instance of value of type T. */
  def arbitrary[T](implicit a: Arbitrary[T]): Gen[T] = a.arbitrary


  // Arbitrary instances for common types

  /** Arbitrary instance of bool */
  implicit lazy val arbBool: Arbitrary[Boolean] = 
    Arbitrary(elements(true,false))

  /** Arbitrary instance of integer */
  implicit lazy val arbInt: Arbitrary[Int] = 
    Arbitrary(sized(s => choose(-s,s)))

  /** Arbitrary instance of Throwable */
  implicit lazy val arbThrowable: Arbitrary[Throwable] =
    Arbitrary(value(new Exception))

  /** Arbitrary instance of Double */
  implicit lazy val arbDouble: Arbitrary[Double] = 
    Arbitrary(sized(s => choose(-s:Double,s:Double)))

  /** Arbitrary instance of char */
  implicit lazy val arbChar: Arbitrary[Char] = 
    Arbitrary(choose(0,255).map(_.toChar))
  
  /** Arbitrary instance of byte */
  implicit lazy val arbByte: Arbitrary[Byte] = 
    Arbitrary(arbitrary[Int].map(_.toByte))

  /** Arbitrary instance of string */
  implicit lazy val arbString: Arbitrary[String] = 
    Arbitrary(arbitrary[List[Char]].map(List.toString(_)))

  /** Arbitrary instance of Gen */
  implicit def arbGen[T](implicit a: Arbitrary[T]): Arbitrary[Gen[T]] =
    Arbitrary(frequency(
      (5, arbitrary[T] map (value(_))),
      (1, Gen.fail)
    ))

  /** Generates an arbitrary property */
  implicit lazy val arbProp: Arbitrary[Prop] = Arbitrary(elementsFreq(
    (5, Prop.proved),
    (4, Prop.falsified),
    (2, Prop.undecided),
    (1, Prop.exception(null))
  ))

  /** Arbitrary instance of test params */
  implicit lazy val arbTestParams: Arbitrary[Test.Params] = Arbitrary(for {
    minSuccTests <- choose(10,150)
    maxDiscTests <- choose(100,500)
    minSize <- choose(0,500)
    sizeDiff <- choose(0,500)
    maxSize <- choose(minSize, minSize + sizeDiff)
  } yield Test.Params(minSuccTests,maxDiscTests,minSize,maxSize,StdRand))

  /** Arbitrary instance of gen params */
  implicit lazy val arbGenParams: Arbitrary[Gen.Params] = Arbitrary(for {
    size <- arbitrary[Int] suchThat (_ >= 0)
  } yield Gen.Params(size, StdRand))
  
  /** Arbitrary instance of option type */
  implicit def arbOption[T](implicit a: Arbitrary[T]): Arbitrary[Option[T]] = 
    Arbitrary(oneOf(value(None), arbitrary[T].map(Some(_))))

  /** Arbitrary instance of List. The maximum length of the list
   *  depends on the size parameter. */
  implicit def arbList[T](implicit a: Arbitrary[T]): Arbitrary[List[T]] = 
    Arbitrary(listOf(arbitrary[T]) map (_.toList))

  /** Arbitrary instance of stream */
  implicit def arbStream[T](implicit a: Arbitrary[T]): Arbitrary[Stream[T]] = 
    Arbitrary(arbitrary[List[T]].map(xs => Stream.fromIterator(xs.elements)))

  /** Arbitrary instance of 2-tuple */
  implicit def arbTuple2[T1,T2](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2]
  ): Arbitrary[(T1,T2)] = Arbitrary(for {
    t1 <- arbitrary[T1]
    t2 <- arbitrary[T2]
  } yield (t1,t2))

  /** Arbitrary instance of 3-tuple */
  implicit def arbTuple3[T1,T2,T3](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3]
  ): Arbitrary[(T1,T2,T3)] = Arbitrary(for {
    t1 <- arbitrary[T1]
    t2 <- arbitrary[T2]
    t3 <- arbitrary[T3]
  } yield (t1,t2,t3))

  /** Arbitrary instance of 4-tuple */
  implicit def arbTuple4[T1,T2,T3,T4](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4]
  ): Arbitrary[(T1,T2,T3,T4)] = Arbitrary(for {
    t1 <- arbitrary[T1]
    t2 <- arbitrary[T2]
    t3 <- arbitrary[T3]
    t4 <- arbitrary[T4]
  } yield (t1,t2,t3,t4))

  /** Arbitrary instance of 5-tuple */
  implicit def arbTuple5[T1,T2,T3,T4,T5](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5]
  ): Arbitrary[(T1,T2,T3,T4,T5)] = Arbitrary(for {
    t1 <- arbitrary[T1]
    t2 <- arbitrary[T2]
    t3 <- arbitrary[T3]
    t4 <- arbitrary[T4]
    t5 <- arbitrary[T5]
  } yield (t1,t2,t3,t4,t5))

  /** Arbitrary instance of 6-tuple */
  implicit def arbTuple6[T1,T2,T3,T4,T5,T6](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5], a6: Arbitrary[T6]
  ): Arbitrary[(T1,T2,T3,T4,T5,T6)] = Arbitrary(for {
    t1 <- arbitrary[T1]
    t2 <- arbitrary[T2]
    t3 <- arbitrary[T3]
    t4 <- arbitrary[T4]
    t5 <- arbitrary[T5]
    t6 <- arbitrary[T6]
  } yield (t1,t2,t3,t4,t5,t6))

}
