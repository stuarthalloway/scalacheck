package org.scalacheck

import Stream.{cons, empty}

sealed abstract class Shrink[T] {
  def shrink(x: T): Stream[T]
}

object Shrink {
  def apply[T](s: T => Stream[T]) = new Shrink[T] {
    override def shrink(x: T) = s(x)
  }

  def shrink[T](x: T)(implicit s: Shrink[T]): Stream[T] = s.shrink(x)

  /** Default shrink instance */
  implicit def shrinkAny[T]: Shrink[T] = Shrink(x => empty)

  /** Shrink instance of integer */
  implicit lazy val shrinkInt: Shrink[Int] = Shrink { n =>

    def iterate[T](f: T => T, x: T): Stream[T] = {
      val y = f(x)
      cons(y, iterate(f,y))
    }

    if(n == 0) empty
    else {
      val ns = cons(0, iterate((_:Int)/2, n).takeWhile(_ != 0).map(n - _))
      if(n < 0) cons(-n,ns) else ns
    }
  }

  /** Shrink instance of Option */
  implicit def shrinkOption[T](implicit s: Shrink[T]): Shrink[Option[T]] =
    Shrink { 
      case None    => empty
      case Some(x) => cons(None, for(y <- shrink(x)) yield Some(y))
    }

  /** Shrink instance of list */
  implicit def shrinkList[T](implicit s: Shrink[T]): Shrink[List[T]] = 
    Shrink { xs =>
      def interleave(xs: Stream[List[T]],ys: Stream[List[T]]): Stream[List[T]] = 
        (xs,ys) match {
          case (xs,ys) if xs.isEmpty => ys
          case (xs,ys) if ys.isEmpty => xs
          case (cons(x,xs),cons(y,ys)) => cons(x, cons(y, interleave(xs,ys)))
        }

      def removeChunks(n: Int, xs: List[T]): Stream[List[T]] = xs match {
        case Nil => empty
        case _::Nil => cons(Nil, empty)
        case _ =>
          val n1 = n / 2
          val n2 = n - n1
          lazy val xs1 = xs.take(n1)
          lazy val xs2 = xs.drop(n1)
          lazy val xs3 =
            for(ys1 <- removeChunks(n1,xs1) if !ys1.isEmpty) yield ys1 ::: xs2
          lazy val xs4 =
            for(ys2 <- removeChunks(n2,xs2) if !ys2.isEmpty) yield xs1 ::: ys2

          cons(xs1, cons(xs2, interleave(xs3,xs4)))
      }

      def shrinkOne(xs: List[T]): Stream[List[T]] = xs match {
        case Nil => empty
        case x::xs =>
          (for(y <- shrink(x)) yield y::xs) append
          (for(ys <- shrinkOne(xs)) yield x::ys)
      }

      removeChunks(xs.length,xs).append(shrinkOne(xs))
    }

  /** Shrink instance of 2-tuple */
  implicit def shrinkTuple2[T1,T2](implicit
    s1: Shrink[T1], s2: Shrink[T2]
  ): Shrink[(T1,T2)] = Shrink { case (t1,t2) =>
    (for(x1 <- shrink(t1)) yield (x1, t2)) append
    (for(x2 <- shrink(t2)) yield (t1, x2))
  }

  /** Shrink instance of 3-tuple */
  implicit def shrinkTuple3[T1,T2,T3](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3]
  ): Shrink[(T1,T2,T3)] = Shrink { case (t1,t2,t3) =>
    println("SHRINKING TUPLE" + (t1,t2,t3))
    (for(x1 <- shrink(t1)) yield (x1, t2, t3)) append
    (for(x2 <- shrink(t2)) yield (t1, x2, t3)) append
    (for(x3 <- shrink(t3)) yield (t1, t2, x3))
  }

  /** Shrink instance of 4-tuple */
  implicit def shrinkTuple4[T1,T2,T3,T4](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4]
  ): Shrink[(T1,T2,T3,T4)] = Shrink { case (t1,t2,t3,t4) =>
    (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4)) append
    (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4)) append
    (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4)) append
    (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4))
  }
}
