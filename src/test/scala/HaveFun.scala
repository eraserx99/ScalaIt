import org.scalatest.FunSuite

abstract class Expr

case class Var(name: String) extends Expr

case class Number(num: Double) extends Expr

case class UnOp(operator: String, arg: Expr) extends Expr

case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

trait MyTrait {
  def msg: String

  override def toString = msg
}

class HaveFun extends FunSuite {

  test("traits, with Incrementing with Filtering") {
    val q1 = (new BasicIntQueue with Incrementing with Filtering)
    q1.put(-1)
    q1.put(0)
    q1.put(1)

    assert(q1.get() == 1)
  }

  test("traits, with Incrementing and Filtering") {
    val q1 = (new BasicIntQueue with Filtering with Incrementing)
    q1.put(-1)
    q1.put(0)
    q1.put(1)

    assert(q1.get() == 0)
  }

  test("match") {
    val op: Expr = UnOp("-", UnOp("-", Var("x")))
    val res: Expr = op match {
      case UnOp("-", UnOp("-", e)) => e
      case BinOp("+", e, Number(0)) => e
      case BinOp("*", e, Number(1)) => e
      case _ => null
    }

    assert(res == Var("x"))
  }

  test("match, variable binding") {
    val res: Expr = UnOp("abs", UnOp("abs", Var("x"))) match {
      case UnOp("abs", e@UnOp("abs", Var("x"))) => e
      case _ => null
    }

    assert(res == UnOp("abs", Var("x")))
  }

  test("List, patterns") {
    val l = List(1, 2, 3, 4, 5, 6)
    val t = l match {
      case e@List(_, _, _) => e
      case _ :: _ :: rest => rest
    }

    assert(t == List(3, 4, 5, 6))
  }

  test("Patterns") {
    def echoWhatYouGaveMe(x: Any): String = x match {
      // constant patterns
      case 0 => "zero"
      case true => "true"
      case "hello" => "you said 'hello'"
      case Nil => "an empty List"

      // sequence patterns
      case List(0, _, _) => "a three-element list with 0 as the first element"
      case List(1, _*) => "a list beginning with 1, having any number of elements"
      case Vector(1, _*) => "a vector starting with 1, having any number of elements"

      // tuples
      case (a, b) => s"got $a and $b"
      case (a, b, c) => s"got $a, $b, and $c"

      // constructor patterns
      case Var(_) => "Var expr"
      case UnOp(_, _) => "UnOp expr"

      // typed patterns
      case s: String => s"you gave me this string: $s"
      case i: Int => s"thanks for the int: $i"
      case f: Float => s"thanks for the float: $f"
      case a: Array[Int] => s"an array of int: ${a.mkString(",")}"
      case as: Array[String] => s"an array of strings: ${as.mkString(",")}"
      case list: List[_] => s"thanks for the List: $list"
      case m: Map[_, _] => m.toString

      // the default wildcard pattern
      case _ => "Unknown"
    }

    assert(echoWhatYouGaveMe(Var("x")) == "Var expr")
  }

  test("Have some fun with PartialFunction!") {
    def fun: PartialFunction[Any, String] = {
      case 0 => "zero"
      case true => "true"
      case "hello" => "you said 'hello'"
      case Nil => "an empty List"
      case _ => "Not acceptable!"
    }

    val v = fun
    /**
      * v is used here to verify that,
      * {
      * case =>
      * case =>
      * ...
      * }
      * represents multiple entry points for a partial function
      */
    val l = v(List())

    assert(l == "an empty List")
  }

  test("append, parametric function") {
    def append[T](xs: List[T], ys: List[T]): List[T] =
      xs match {
        case List() => ys
        case x :: xs1 => x :: append(xs1, ys)
      }

    val l = append(List(1, 2), List(3, 4))

    assert(l == List(1, 2, 3, 4))
  }

  test("trait, instantiation") {
    /**
      * new MyTrait{} instantiates an anonymous class mixing in MyTrait
      */
    val mt = new MyTrait {
      val msg = "Hello World!"
    }

    assert(mt.toString == "Hello World!")
  }

  test("trait, pre-initialized field with anonymous class") {
    val mt = new {
      val msg = "Hello World!"
    } with MyTrait

    assert(mt.toString == "Hello World!")
  }

  test("trait, pre-initialized field with class") {
    class MyClass(message: String) extends {
      val msg = message
    } with MyTrait {
      override def toString: String = msg
    }

    val mc = new MyClass("Hello World!")

    assert(mc.toString == "Hello World!")
  }

  test("type") {
    class Food
    abstract class Animal {
      type SuitableFood <: Food

      def eat(food: SuitableFood)
    }

    class Grass extends Food
    class Cow extends Animal {
      type SuitableFood = Grass

      def eat(food: SuitableFood): Unit = {
      }
    }

    class Fish extends Food

    assertTypeError("(new Cow).eat(new Fish)");
  }

  test("path-dependent types") {
    class Food
    abstract class Animal {
      type SuitableFood <: Food

      def eat(food: SuitableFood)
    }

    class DogFood extends Food
    class Dog extends Animal {
      type SuitableFood = DogFood

      def eat(food: SuitableFood): Unit = {
      }
    }

    assertCompiles("val d1 :: d2 :: rest = List(new Dog, new Dog); d1.eat(new d2.SuitableFood)");
  }

  test("Inner class, one does not work!") {
    class Outer {

      class Inner

    }

    /**
      * In Java, you use Outer.Inner. In Scala, you use Outer#Inner
      * However, Outer#Inner does not represent a type. You have to
      * have an object associated.
      */
    assertDoesNotCompile("val v = new Outer#Inner")
  }

  test("Inner class, one does work!") {
    class Outer {

      class Inner

    }

    /**
      * In Java, you use Outer.Inner. In Scala, you use Outer#Inner
      * However, Outer#Inner does not represent a type. You have to
      * have an object associated.
      */
    assertCompiles("val o = new Outer; val i = new o.Inner")
  }

  test("Inner class, path-dependent!") {
    class Outer {

      class Inner

    }

    assertDoesNotCompile("val o1 = new Outer; val o2 = new Outer; var i1 = new o1.Inner; i1 = new o2.Inner")
  }

  test("Inner class, one works!") {
    class Outer {

      class Inner

    }

    assertCompiles("val o = new Outer; val i = new o.Inner")
  }

  test("refined types") {
    class Food
    abstract class Animal {
      type SuitableFood <: Food

      def eat(food: SuitableFood)
    }

    class DogFood extends Food

    /**
      * It might be too much trouble to make a subtype of Animal to represent
      * Animal that easts DogFood. In Scala, you can use "refined types" to
      * archive the results. Here we use Animal { type SuitableFood = DogFood } to
      * represent a type of Animal that east DogFood.
      */
    assertCompiles("var animals: List[Animal { type SuitableFood = DogFood}] = Nil")
  }

  test("Enumeration basics") {
    object Direction extends Enumeration {
      val North, East, South, West = Value
    }

    assert(1 == Direction.East.id)
  }

  test("Enumeration is path-dependent") {
    object Direction extends Enumeration {
      val North, East, South, West = Value
    }
    object Animal extends Enumeration {
      val Dog, Cat, Horse = Value
    }

    assert(Direction.North != Animal.Dog)
  }
}
