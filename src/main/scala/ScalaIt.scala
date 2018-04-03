import scala.collection.mutable.ArrayBuffer

abstract class IntQueue {
  def get(): Int

  def put(x: Int)
}

class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]

  def get() = buf.remove(0)

  def put(x: Int) = buf += x
}

/**
  * super calls in a trait are dynamically bound
  * the super call in trait Doubling will work so long as the trait is mixed in after
  * another trait or class that gives a concrete definition to the method.
  */
trait Doubling extends IntQueue {
  abstract override def put(x: Int) = super.put(2 * x)
}

trait Incrementing extends IntQueue {
  abstract override def put(x: Int) = super.put(x + 1)
}

trait Filtering extends IntQueue {
  abstract override def put(x: Int) = if ( x >= 0) super.put(x)
}

