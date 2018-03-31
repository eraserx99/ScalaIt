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

object ScalaIt {
  def main(args: Array[String]): Unit = {
    val q1 = (new BasicIntQueue with Incrementing with Filtering)
    q1.put(-1)
    q1.put(0)
    q1.put(1)

    println("====== q1 ======")
    println(q1.get())
    println(q1.get())

    val q2 = (new BasicIntQueue with Filtering with Incrementing)
    q2.put(-1)
    q2.put(0)
    q2.put(1)

    println("====== q2 ======")
    println(q2.get())
    println(q2.get())
  }
}
