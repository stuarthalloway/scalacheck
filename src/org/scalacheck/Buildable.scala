package org.scalacheck

trait Builder[C[_], T] {
  def +=(x: T)
  def finalise: C[T]
}

trait Buildable[C[_]] {
  def builder[T]: Builder[C,T]
}

object Buildable {

  implicit object buildableList extends Buildable[List] {
    def builder[T] = new Builder[List,T] {
      val buf = new scala.collection.mutable.ListBuffer[T]
      def +=(x: T) = buf += x
      def finalise = buf.toList
    }
  }

  implicit object buildableStream extends Buildable[Stream] {
    def builder[T] = new Builder[Stream,T] {
      var stream: Stream[T] = Stream.empty
      def +=(x: T) = stream = Stream.cons(x, stream)
      def finalise = stream.reverse
    }
  }

  implicit object buildableArray extends Buildable[Array] {
    def builder[T] = new Builder[Array,T] {
      val buf = new scala.collection.mutable.ArrayBuffer[T]
      def +=(x: T) = buf += x
      def finalise = {
        val arr = new Array[T](buf.size)
        buf.copyToArray(arr, 0)
        arr
      }
    }
  }

}
