package org.scalacheck

trait FreqMap[T] {
  protected val underlying: scala.collection.immutable.Map[T,Int]
  val total: Int
 
  def +(t: T) = new FreqMap[T] {
    val n = FreqMap.this.underlying.get(t) match {
      case None => 1
      case Some(n) => n+1
    }
    val underlying = FreqMap.this.underlying + (t -> n)
    val total = FreqMap.this.total + 1
  }
 
  def getCount(t: T) = underlying.get(t)
 
  def getCounts: Collection[(T,Int)] = underlying
 
  def getRatio(t: T) = for(c <- getCount(t)) yield (c: Float)/total
 
  def getRatios = for((t,c) <- underlying) yield (t, (c: Float)/total)

  override def toString = underlying.toString
}
 
object FreqMap {
  def empty[T] = new FreqMap[T] {
    val underlying = scala.collection.immutable.Map.empty[T,Int]
    val total = 0
  }
}
